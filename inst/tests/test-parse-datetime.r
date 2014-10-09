context("parse_datetime")
library(timeDate)

check_date <- function(date, expectation, mode='since') {
  df <- data.frame(x = date, y = 'bleh')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(parse_datetime)
  mb$run(mp, 1, mode=mode)
  eval(substitute(expect_equal(expectation, mp$data$x)))
}

datetime = '1991-12-11 03:14:15.9265'
date = '1991-12-11'
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
