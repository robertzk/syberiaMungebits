#' Take input in a wide variety of date/time formats and output it in a variety of consistent formats.
#'
#' @param input contains the date to be formatted
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

swap_month <- function(input) {
  output <- tolower(input)
  output <- gsub('january', '01', output)
  output <- gsub('jan', '01', output)
  output <- gsub('february', '02', output)
  output <- gsub('feb', '02', output)
  output <- gsub('march', '03', output)
  output <- gsub('mar', '03', output)
  output <- gsub('april', '04', output)
  output <- gsub('apr', '04', output)
  output <- gsub('may', '05', output)
  output <- gsub('june', '06', output)
  output <- gsub('jun', '06', output)
  output <- gsub('july', '07', output)
  output <- gsub('jul', '07', output)
  output <- gsub('august', '08', output)
  output <- gsub('aug', '08', output)
  output <- gsub('september', '09', output)
  output <- gsub('sep', '09', output)
  output <- gsub('october', '10', output)
  output <- gsub('oct', '10', output)
  output <- gsub('november', '11', output)
  output <- gsub('nov', '11', output)
  output <- gsub('december', '12', output)
  output <- gsub('dec', '12', output)
  output
}

swap_spaces <- function(input) {
  gsub(' ', '-', input)
}

convert_incoming <- function(input) {
  # Standardize from many inputs
  input = input[[1]]
  date = tryCatch({
    if (is.numeric(input)) { as.Date(input, origin='1970/1/1') }
    else if (is.character(input)) { as.Date(input) }
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
    date = convert_outgoing(convert_incoming(input), 1)
  }
  else {
    convert_outgoing(convert_incoming(swap_spaces(swap_month(input))), mode)
  }
}

#' @export
timekeeper <- column_transformation(timekeeper_fn, mutating = TRUE, named = TRUE)
