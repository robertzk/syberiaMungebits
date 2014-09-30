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

timekeeper_fn <- function(input, mode="date") {
  # Standardize from many inputs
  input = input[[1]]
  if (is.numeric(input)) { date = as.Date(input, origin='1970/1/1') }
  else if (is.character(input)) {
    if (!is.na(grep("-", input) || grep("\\/", input))) { date = as.Date(input) }
    else { return("NA") }
  }
  else { return("NA") }

  # Pipe to many outputs
  if (mode == "numeric") { date = as.numeric(date) }
  date
}

#' @export
timekeeper <- column_transformation(timekeeper_fn, mutating = TRUE, named = TRUE)
