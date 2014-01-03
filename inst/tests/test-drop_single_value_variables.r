context("drop_single_value_variables_fn")

test_that("it correctly drops a 1-value column", {
  require(mungebits)
  dataframe <- data.frame(cbind(rep(1, 1000), rep(c(1,2), 500)))
  mb <- mungebits:::mungebit(drop_single_value_variables)
  mp <- mungebits:::mungeplane(dataframe)
  mb$run(mp)
  dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  mp2 <- mungebits:::mungeplane(dataframe)
  mb$run(mp2)
  expect_equal(mp$data, mp2$data)
})

test_that("it correctly keeps a 2-value column", {
 #  column <- rep(c(1, 2), 1000)
 #  expect_equal(column, drop_single_value_variables_fn(column))
})

test_that("it correctly drops an empty column", {
 # expect_equal(NULL, drop_single_value_variables_fn(character(0)))
})

