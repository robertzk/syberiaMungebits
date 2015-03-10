#' A simpler, faster mungebit that takes Postgresql CreatedAt statements in the format of YYYY-MM-DD HH:MM:SS.SSSS and convert them into the choice of...
#'    days since 1970 (default)
#'    the hour of day (0-23, use mode="hod")
#'    the day of week (Mon-Sun, use mode="dow")
#'    the day of month (1-31, use mode="dom")
#'    the day of year (1-365, use mode="doy")
#'    the month of year (1-12 corresponding to Jan-Dec, use mode="moy") 
#'    if the output is a holiday (\code{TRUE} or \code{FALSE}, use mode="holiday")
#'    if the output is a weekend (\code{TRUE} or \code{FALSE}, use mode="weekend")
#'    if the output is a business day (\code{TRUE} or \code{FALSE}, use mode="bizday")

#' @param date contains the date to be formatted
#' @param mode gives the desired output format (see above)
datetime_fn <- function(createdat, mode="since", permissive=FALSE) {
  
  # track which records are NA before the mungebit
  originally_NA <- is.na(createdat)
  
  # do the conversion
  datetime <- suppressWarnings(lubridate:::ymd_hms(createdat))
  if (any(is.na(datetime))) {
    if (mode == 'hod') { stop('Cannot extract hour from date-only.') }
    datetime <- suppressWarnings(lubridate:::ymd(createdat))
  }
  
  # check to see if any records are converted to NA
  if (any(is.na(datetime[!originally_NA])) && !isTRUE(permissive)) { 
    stop('Improper date format.') 
  }

  suppressWarnings(switch(mode,
    "since" = as.numeric(as.Date(datetime)),
    "hod" = lubridate:::hour(datetime),
    "dow" = weekdays(datetime),
    "dom" = lubridate:::day(datetime),
    "doy" = syberiaMungebits:::get.doy(datetime),
    "moy" = lubridate:::month(datetime),
    "holiday" = syberiaMungebits:::is.holiday(datetime),
    "weekend" = syberiaMungebits:::is.weekend(datetime),
    "bizday" = !syberiaMungebits:::is.holiday(datetime) & !syberiaMungebits:::is.weekend(datetime),
    date
  ))
}

get.doy <- function(date) {
  thisyear <- lubridate:::year(date)
  janprime <- as.Date(paste(c(thisyear,'1','1'), collapse='-'))
  as.numeric(as.Date(date)) - as.numeric(janprime) + 1
}

is.weekend <- function(date) {
  weekdays(date) == 'Saturday' | weekdays(date) == 'Sunday'
}

is.holiday <- function(date) {
  year <- lubridate:::year(as.Date(date))
  holidays_fun <- list(ChristmasDay, USNewYearsDay, USMemorialDay, LaborDay, USThanksgivingDay)
  fun <- function(f, ...) { f(...) }
  holidays <- lapply(lapply(holidays_fun, fun, year), as.Date)
  as.Date(date) %in% unlist(holidays)
}

#' @export
parse_datetime <- column_transformation(syberiaMungebits:::datetime_fn, mutating = TRUE)
