#' Take input in a wide variety of date/time formats and output it in a variety of consistent formats.
#'
#' @param input contains the date to be formatted
#'
#' Makes five assumptions about dates:
#'  - 12/11/1991 will be December 11, 1991 (US dates)
#'  - two-digit years > 20 refer to 20th century ('91 refers to 1991)
#'  - two-digit years <= 20 refer to 21st century ('14 refers to 2014)
#'  - two-digit years will be at end (12/11/20 will refer to December 11, 1920)
#'  - numeric strings are dates if length 8, days since 1970 otherwise
#'  - numeric strings as dates will be YYYYMMDD
#'
#' @param mode tells the desired output format
#'   "date" will return a R date
#'   "numeric" will return the number of days since Jan 1, 1970
#'   "holiday" will return TRUE if it's a US federal holiday and FALSE if not
#'   "weekend" will return TRUE if it's a weekend and FALSE if not
#'   "businessday" will return TRUE if it's a weekend or holiday
datekeeper_fn <- function(input, mode="date") {
  input = input[[1]]
  Ramd::packages('timeDate')
  if(is.numeric(input)) {
    date = syberiaMungebits:::convert_outgoing(syberiaMungebits:::convert_incoming(input), mode)
  }
  else {
    date = syberiaMungebits:::standardize_dividers(input)
    date = syberiaMungebits:::remove_punctuation(date)
    date = syberiaMungebits:::handle_two_digit_years(date)
    date = syberiaMungebits:::handle_order(date)
    date = syberiaMungebits:::handle_month(date)
    date = syberiaMungebits:::convert_incoming(date)
    date = syberiaMungebits:::convert_outgoing(date, mode)
    date
  }
}

standardize_dividers <- function(input) {
  output = tolower(input)
  output = gsub(' ', '-', output)
  output = gsub('\\/', '-', output)
  output = gsub('([a-z])(?=[0-9])', '\\1-', output, perl=T)
  output = gsub('([0-9])(?=[a-z])', '\\1-', output, perl=T)
  splits = strsplit(output, "-")
  if (length(splits[[1]]) == 2 && nchar(splits[[1]][2]) == 6) {
    output = paste0(splits[[1]][1], '-', substr(splits[[1]][2],0,2), '-', substr(splits[[1]][2],3,7))
  }
  output
}

remove_punctuation <- function(input) {
  gsub('[^a-zA-Z0-9-]', '', input)
}

handle_order <- function(input) {
  strips = strsplit(input, "-")[[1]]
  if (nchar(strips[3]) == 4) { output = paste0(strips[3], "-", strips[1], "-", strips[2]) }
  else { output = input }
  output
}

handle_two_digit_years <- function(input) {
  strips = strsplit(input, "-")[[1]]
  if (length(strips) != 3) { return("NA") }
  for (strip in strips) {
    if (nchar(strip) == 4) { return(input) }
  }
  if (as.numeric(strips[3]) > 20) { year_beginning = 19 }
  else { year_beginning = 20 }
  paste0(strips[1], '-', strips[2], '-', year_beginning, strips[3])
}

handle_month <- function(input) {
  if (is.character(input) && input != "NA") {
    months = c('jan', 'feb', 'mar', 'apr', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    strips = strsplit(input, "-")[[1]]
    if (length(strips) != 3) { return("NA") }
    for (month in months) {
      if (grepl(month, strips[2])) { input = as.Date(input, '%Y-%B-%d') }
      if (grepl(month, strips[3])) { input = as.Date(input, '%Y-%d-%B') }
    }
  }
  input
}

convert_incoming <- function(input) {
  input = input[[1]]
  date = tryCatch({
    if (is.numeric(input)) { syberiaMungebits:::handle_numeric(input) }
    else if (is.character(input)) { as.Date(input) }
    else if (class(input) == 'Date') { input }
    else { "NA" }
  }, error = function(cond) { return("NA") })
  date
}

convert_outgoing <- function(input, mode="date") {
  output = "NA"
  if (as.character(input) != "NA") {
    if (mode == "numeric") { output = as.numeric(input) }
    else if (mode == "holiday") { output = syberiaMungebits:::is.holiday(input) }
    else if (mode == "weekend") { output = syberiaMungebits:::is.weekend(input) }
    else if (mode == "businessday") { output = !syberiaMungebits:::is.holiday(input) && !syberiaMungebits:::is.weekend(input) }
    else { output = input }
  }
  output
}

is.weekend <- function(date) {
  weekdays(date) == 'Saturday' || weekdays(date) == 'Sunday'
}

is.holiday <- function(date) {
  year = as.numeric(format(date, '%Y'))
  any(is.element(date, sapply(list(ChristmasDay, USNewYearsDay, USMemorialDay, USIndependenceDay, LaborDay, USThanksgivingDay), function(fn) as.Date(fn(year)))))
}

handle_numeric <- function(input) {
  if (nchar(input) != 8) { output = as.Date(input, origin='1970/1/1') }
  else {
    output = as.Date(paste0(substring(input,0,4), '-', substring(input,5,6), '-', substring(input,7,8)))
  }
  output
}

#' @export
datekeeper <- column_transformation(syberiaMungebits:::datekeeper_fn, mutating = TRUE, named = TRUE)
