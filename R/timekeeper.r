#' Take input in a wide variety of date/time formats and output it in a variety of consistent formats.
#'
#' @param input contains the date to be formatted
#'
#' @param mode tells the desired output format
#'   "date" will return a R date
#'   "numeric" will return the number of days since Jan 1, 1970
#'   "holiday" will return TRUE if it's a US federal holiday and FALSE if not
#'   "weekend" will return TRUE if it's a weekend and FALSE if not
#'   "holidayweekend" will return TRUE if it's a weekend or holiday

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
  if (as.character(date) != "NA") {
    if (mode == "numeric") { date = as.numeric(date) }
    else if (mode == "holiday") { date = is.holiday(date) }
    else if (mode == "weekend") { date = is.weekend(date) }
    else if (mode == "holidayweekend") { date = is.holiday(date) || is.weekend(date) }
  }
  date
}

timekeeper_fn <- function(input, mode="date") {
  input = input[[1]]
  convert_outgoing(convert_incoming(input), mode=mode)
}

#' @export
timekeeper <- column_transformation(timekeeper_fn, mutating = TRUE, named = TRUE)
