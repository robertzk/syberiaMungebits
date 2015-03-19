context('rm_monotonic')

test_that("it drops monotonically related variables", {
  x <- 1:10
  y <- x^2
  z <- rep(c(0,1), 5)
  df <- data.frame(x,y,z)
  remove_monotonic(df, 1)
  expect_equal(df, data.frame(x,z))
})
