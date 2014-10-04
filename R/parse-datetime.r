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
datetime_fn <- function(createdat, mode="since") {
  datetime = lubridate:::ymd_hms(createdat)
  switch(mode,
    "since" = as.numeric(as.Date(datetime)),
    "hod" = lubridate:::hour(datetime),
    "dow" = weekdays(datetime),
    "dom" = lubridate:::day(datetime),
    "doy" = syberiaMungebits:::get.doy(datetime),
    "moy" = lubridate:::month(datetime),
    "holiday" = syberiaMungebits:::is.holiday(datetime),
    "weekend" = syberiaMungebits:::is.weekend(datetime),
    "bizday" = !syberiaMungebits:::is.holiday(datetime) && !syberiaMungebits:::is.weekend,
    date
  )
}

get.doy <- function(date) {
  thisyear = lubridate:::year(date)
  janprime = as.Date(paste(c(thisyear,'1','1'), collapse='-'))
  as.numeric(as.Date(date)) - as.numeric(janprime) + 1
}

is.weekend <- function(date) {
  weekdays(date) == 'Saturday' || weekdays(date) == 'Sunday'
}

is.holiday <- function(date) {
  year = lubridate:::year(as.Date(date))
  any(is.element(as.Date(date), sapply(list(ChristmasDay,
                                   USNewYearsDay,
                                   USMemorialDay,
                                   USIndependenceDay,
                                   LaborDay,
                                   USThanksgivingDay),
                              function(fn) as.Date(fn(year)))))
}

#' @export
parse_datetime <- column_transformation(syberiaMungebits:::datetime_fn, mutating = TRUE)
