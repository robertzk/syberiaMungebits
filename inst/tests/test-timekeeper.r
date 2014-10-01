context("timekeeper")

test_that("it converts YYYY-MM-DD to date", {
  df <- data.frame(x='1991-12-11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date('1991-12-11'), mp$data$x)
})

test_that("it converts YYYY/MM/DD to date", {
  df <- data.frame(x='1991/12/11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date('1991/12/11'), mp$data$x)
})

test_that("it converts YYYY MM DD to date", {
  df <- data.frame(x='1991 12 11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date('1991/12/11'), mp$data$x)
})

test_that("it converts YYYY [Written Month] DD to date", {
  df <- data.frame(x='1991 Dec 11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date('1991/12/11'), mp$data$x)
})

test_that("it converts 1000 to date", {
  df <- data.frame(x=1000, y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date('1972/09/27'), mp$data$x)
})

test_that("it converts garbage to NA", {
  df <- data.frame(x='garbage', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal('NA', mp$data$x)
})

test_that("it converts to numeric date", {
  df <- data.frame(x='1991-12-11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1, mode="numeric")
  expect_equal(8014, mp$data$x)
})

test_that("it converts to TRUE in is weekend mode if weekend", {
  df <- data.frame(x='2014-10-04', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1, mode="weekend")
  expect_equal(TRUE, mp$data$x)
})

test_that("it converts to FALSE in is weekend mode if not weekend", {
  df <- data.frame(x='2014-09-30', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1, mode="weekend")
  expect_equal(FALSE, mp$data$x)
})

test_that("it converts to TRUE in is holiday mode if holiday", {
  df <- data.frame(x='2012-11-22', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1, mode="holiday")
  expect_equal(TRUE, mp$data$x)
})

test_that("it converts to FALSE in is holiday mode if not holiday", {
  df <- data.frame(x='2012-11-21', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1, mode="holiday")
  expect_equal(FALSE, mp$data$x)
})

