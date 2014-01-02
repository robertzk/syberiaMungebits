context(".drop_single_value_variables")

test_that("it correctly drops a 1-value column", {
  column <- rep(1, 1000)
  expect_equal(NULL, .drop_single_value_variables(column))
})

test_that("it correctly keeps a 2-value column", {
  column <- rep(c(1, 2), 1000)
  expect_equal(column, .drop_single_value_variables(column))
})

test_that("it correctly drops an empty column", {
  expect_equal(NULL, .drop_single_value_variables(character(0)))
})

