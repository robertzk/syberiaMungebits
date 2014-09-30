context("timekeeper")

test_that("it converts YYYY-MM-DD to date", {
  df <- data.frame(x='1991-12-11', y='blah')
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(timekeeper)
  mb$run(mp)
  expect_equal(as.Date("1991-12-11"), mp$data)
})
