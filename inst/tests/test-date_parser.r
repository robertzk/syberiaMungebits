context("date_parser_fn")

test_that("it correctly parses invalid dates", {
  c("", "no days")
  c(NA_integer_, NA_integer_)
  expect_equal(c(NA_integer_, NA_integer_), date_parser_fn(c("", "no days")))
})

test_that("it correctly parses numeric dates", {
  expect_equal(c(8, 0, 100),
               date_parser_fn(c("8 days", "0 days", "100 days")))
})

test_that("it correctly parses a mix of valid and invalid dates", {
  expect_equal(c(8, 0, 100, NA_integer_, NA_integer_),
    date_parser_fn(c("8 days", "0 days", "100 days", "", "no days")))
})
