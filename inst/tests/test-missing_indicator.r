context("missing_indicator")

test_that("it indicates missings with a new column and fills in the original column", {
  
  train.x <- c(1,2,NA,NA,3,4,NA,1)
  train.df <- data.frame(x=train.x)
  mp <- mungebits:::mungeplane(train.df)
  
  mb <- mungebits:::mungebit(missing_indicator)
  mb$run(mp,'x',c('a','b','x'))

  expect_equal(mp$data$a,c(1,2,1,1,3,4,1,1),info="did not fill in NAs in the original variable")
  expect_equal(mp$data$b,c(0,0,1,1,0,0,1,0),info="did not successfully indicate missing variables")
  
})
