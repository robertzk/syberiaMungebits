context('remove_monotonic')

test_that("it drops monotonically related variables", {
  x <- 1:10
  y <- x^2
  z <- rep(c(0,1), 5)
  df <- data.frame(x,y,z)
  
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(remove_monotonic)
  mb$run(mp, 0.9)

  expect_equal(mp$data, data.frame[,c('x','z')])
})
