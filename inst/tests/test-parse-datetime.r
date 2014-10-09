context("parse_datetime")
library(timeDate)

check_date <- function(date, expectation, mode='since') {
  mb <- set_mungebit()
  mp <- set_mungeplane(date)
  mb$run(mp, 1, mode=mode)
  eval(substitute(expect_equal(expectation, mp$data$x)))
}

check_invalid_date <- function(date, expectation, mode='since') {
  mb <- set_mungebit()
  mp <- set_mungeplane(date)
  expect_error(mb$run(mp, 1, mode=mode), expectation)
}

set_mungebit <- function() {
  mungebits:::mungebit(parse_datetime)
}

set_mungeplane <- function(date) {
  df <- data.frame(x = date, y = 'bleh')
  mungebits:::mungeplane(df)
}

datetime = '1991-12-11 03:14:15.9265'
date = '1991-12-11'
multiple_datetimes = c('1991-01-01 12:34:56.789','1992-01-01 12:34:56.789','1993-01-01 12:34:56.789')
times_and_datetimes = c('1991-01-01','1992-01-01 12:34:56.789','1993-01-01 12:34:56.789', '1994-01-01')
test_that("it converts to numeric date", { check_date(datetime, 8014) })
test_that("it converts to hour of day", { check_date(datetime, 3, 'hod') })
test_that("it converts to day of week", { check_date(datetime, 'Wednesday', 'dow') })
test_that("it converts to day of month", { check_date(datetime, 11, 'dom') })
test_that("it converts to day of year", { check_date(datetime, 345, 'doy') })
test_that("it converts to month of year", { check_date(datetime, 12, 'moy') })
test_that("it converts to TRUE in is weekend mode if weekend", { check_date('2014-10-04 11:22:33.4455', TRUE, 'weekend') })
test_that("it converts to FALSE in is weekend mode if not weekend", { check_date('2014-09-30 12:34:56.7890', FALSE, 'weekend') })
test_that("it converts to TRUE in is holiday mode if holiday", { check_date('2014-11-27 11:22:33.4455', TRUE, 'holiday') })
test_that("it converts to FALSE in is holiday mode if not holiday", { check_date('2012-11-21 12:34:56.7890', FALSE, 'holiday') })
test_that("it works on dates too", { check_date(date, 8014) })
test_that("it works on multiple datetimes", { check_date(multiple_datetimes, 'blah') })
# test_that("it works on multiple times and datetimes", { check_date(times_and_datetimes) })
test_that("it returns an error if time format is invalid", { check_invalid_date('pizza', 'Improper date') })
test_that("it returns an error if trying to extract hour from date-only", { check_invalid_date('1991-12-11', 'Cannot extract hour', 'hod') })
