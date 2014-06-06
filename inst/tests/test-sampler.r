context("sampler")

test_that("it upsamples 1s", {
    
  # parameters controlling degree of imbalance and upsampling ratio
  n0 <- 100
  n1 <- 10
  frac <- 2
  
  # create the data set as a mungeplane
  y <- c(rep(0,n0),rep(1,n1))
  x <- rnorm(n=n0+n1)
  df <- data.frame(y,x)
  mp <- mungebits:::mungeplane(df)
  
  # execute the mungeplane
  mb <- mungebits:::mungebit(sampler)
  mb$run(mp,var='y',val=1,frac=frac)
  m1 <- sum(mp$data$y==1)
  expect_equal(round(frac*n1), m1, info="Upsampling did not succeed")
  
})