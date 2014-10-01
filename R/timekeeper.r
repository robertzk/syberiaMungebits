#' Take input in a wide variety of date/time formats and output it in a variety of consistent formats.
#'
#' @param input contains the date to be formatted
#'
#' Makes four assumptions about dates:
#'  - 12/11/1991 will be December 11, 1991 (US dates)
#'  - two-digit years > 20 refer to 20th century ('91 refers to 1991)
#'  - two-digit years <= 20 refer to 21st century ('14 refers to 2014)
#'  - numeric strings are dates if length 8, days since 1970 otherwise
#'
#' @param mode tells the desired output format
#'   "date" will return a R date
#'   "numeric" will return the number of days since Jan 1, 1970
#'   "holiday" will return TRUE if it's a US federal holiday and FALSE if not
#'   "weekend" will return TRUE if it's a weekend and FALSE if not
#'   "businessday" will return TRUE if it's a weekend or holiday

is.weekend <- function(date) {
  weekdays(date) == 'Saturday' || weekdays(date) == 'Sunday'
}

is.holiday <- function(date) {
  year = as.numeric(format(date, '%Y'))
  if (date == as.Date(ChristmasDay(year))) { return(TRUE) }
  if (date == as.Date(USNewYearsDay(year))) { return(TRUE) }
  if (date == as.Date(USMemorialDay(year))) { return(TRUE) }
  if (date == as.Date(USIndependenceDay(year))) { return(TRUE) }
  if (date == as.Date(LaborDay(year))) { return(TRUE) }
  if (date == as.Date(USThanksgivingDay(year))) { return(TRUE) }
  FALSE
}

handle_order <- function(input) {
  strips = strsplit(input, "-")
  if (nchar(strips[[1]][3]) == 4) { output = paste0(strips[[1]][3], "-", strips[[1]][1], "-", strips[[1]][2]) }
  else { output = input }
  output
}

handle_month <- function(input) {
  if (is.character(input) && input != "NA") {
    months = c('jan', 'feb', 'mar', 'apr', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    for (month in months) {
      if (grepl(month, input)) {
        input = as.Date(input, '%Y-%B-%d')
      }
    }
  }
  input
}

standardize_dividers <- function(input) {
  output = gsub(' ', '-', input)
  output = gsub('\\/', '-', output)
  tolower(output)
}

convert_incoming <- function(input) {
  # Standardize from many inputs
  input = input[[1]]
  date = tryCatch({
    if (is.numeric(input)) { as.Date(input, origin='1970/1/1') }
    else if (is.character(input)) { as.Date(input) }
    else if (class(input) == 'Date') { input }
    else { "NA" }
  }, error = function(cond) { return("NA") })
  date
}

convert_outgoing <- function(input, mode="date") {
  # Pipe to many outputs
  output = "NA"
  if (as.character(input) != "NA") {
    if (mode == "numeric") { output = as.numeric(input) }
    else if (mode == "holiday") { output = is.holiday(input) }
    else if (mode == "weekend") { output = is.weekend(input) }
    else if (mode == "businessday") { output = !is.holiday(input) && !is.weekend(input) }
    else { output = input }
  }
  output
}

timekeeper_fn <- function(input, mode="date") {
  input = input[[1]]
  Ramd::packages('timeDate')
  if(is.numeric(input)) {
    date = convert_outgoing(convert_incoming(input), mode)
  }
  else {
    date = standardize_dividers(input)
    date = handle_order(date)
    date = handle_month(date)
    date = convert_incoming(date)
    date = convert_outgoing(date, mode)
    date
  }
}

#' @export
timekeeper <- column_transformation(timekeeper_fn, mutating = TRUE, named = TRUE)
