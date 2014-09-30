#' Take input in a wide variety of date/time formats and output it in a variety of consistent formats.
#'
#' @param formatdate contains the date to be formatted
#'
#' @param mode tells the desired output format
#'   "date" will return a R date
#'   "numeric" will return the number of days since Jan 1, 1970
#'   "holiday" will return TRUE if it's a US federal holiday and FALSE if not
#'   "weekend" will return TRUE if it's a weekend and FALSE if not
#'   "holidayweekend" will return TRUE if it's a weekend or holiday

timekeeper_fn <- function(formatdate, mode="date") {
  date = as.Date(formatdate[[1]])
  if (mode == "numeric") { date = as.numeric(date) }
  date
}

#' @export
timekeeper <- column_transformation(timekeeper_fn, mutating = TRUE, named = TRUE)
