context("timekeeper")

test_that("it converts YYYY-MM-DD to date", {
  df <- data.frame(x='1991-12-11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date("1991-12-11"), mp$data$x)
})

test_that("it converts YYYY/MM/DD to date", {
  df <- data.frame(x='1991/12/11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date("1991/12/11"), mp$data$x)
})

test_that("it converts 1000 to date", {
  df <- data.frame(x=1000, y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1)
  expect_equal(as.Date("1972/09/27"), mp$data$x)
})

test_that("it converts to numeric date", {
  df <- data.frame(x='1991-12-11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp, 1, mode="numeric")
  expect_equal(8014, mp$data$x)
})
