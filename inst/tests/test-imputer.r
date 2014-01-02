context("imputer")

test_that("it imputes a column in a dataframe correctly", {
  require("mungebits")
  iris2 <- iris
  mungebit(imputer)
})
