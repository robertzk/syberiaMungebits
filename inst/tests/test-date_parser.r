context(".date_parser")

test_that("it correctly parses invalid dates", {
  column <- c("", "no days")
  expect_equal(c(NA_integer_, NA_integer_), .date_parser(column))
})

test_that("it correctly parses numeric dates", {
  expect_equal(c(8, 0, 100),
               .date_parser(c("8 days", "0 days", "100 days")))
})

test_that("it correctly parses a mix of valid and invalid dates", {
  column <- c("8 days", "0 days", "100 days", "", "no days")
  expect_equal(c(8, 0, 100, NA_integer_, NA_integer_), .date_parser(column))
})
