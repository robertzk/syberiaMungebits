context("truncator")

test_that("it caps the prediction data at the min/max of the training data", {
  
  train.x <- 1:10
  train.y <- rep(0,10)
  train.df <- data.frame(x=train.x,y=train.y)
  mp1 <- mungebits:::mungeplane(train.df)
  
  test.x <- train.x+0.5
  test.y <- -4:5
  test.df <- data.frame(x=test.x,y=test.y)
  mp2 <- mungebits:::mungeplane(test.df)
  
  mb <- mungebits:::mungebit(truncator)
  mb$run(mp1)
  mb$run(mp2)
  
  compare <- data.frame(
    x = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.0),
    y = rep(0,10)
  )
  
  expect_equal(compare, 
               mp2$data,                                        
               info = paste0("The thing didn't work"))    
  
})
