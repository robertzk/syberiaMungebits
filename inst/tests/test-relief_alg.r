context("relief_alg")

example <- function() {
  df <- iris
  df$noise <- pi * seq_len(nrow(df))
  df$dep_var <- as.integer(df$Species == df$Species[1])
  df
}

test_that("it does not crash", {
  
  # create a random 200x5 dataframe with 4 numeric columns and one factor, plus missing values
  dataframe <- list()
  dataframeNoMissings <- list()
  for (i in 1:5) {
    
    varname <- paste0('var',i)
    
    # 4 numeric columns and 1 factor column
    if (i==3) {
      dataframe[[varname]] <- sample(LETTERS[1:5], size=200, replace=TRUE)
    } else {
      dataframe[[varname]] <- rnorm(200)
    }
    
    # backup copy before inserting NAs
    dataframeNoMissings[[varname]] <- dataframe[[varname]]    
    
    # make some rows missing
    na.rows <- sample.int(n=200, size=20)
    dataframe[[varname]][na.rows] <- NA 
  }
  dataframeNoMissings <- as.data.frame(dataframeNoMissings)
  dataframe <- as.data.frame(dataframe)
  
  # choose one of the columns at random as your sole "true" predictor
  theOneTruePredictor <- sample.int(5,1)
  if (theOneTruePredictor==3) {
    p <- ifelse(dataframeNoMissings$var3=='C', 0.99, 0.01)
  } else {
    p <- 1/(1 + exp(-5*scale(dataframeNoMissings[,theOneTruePredictor])))
  }
  dataframe$dep_var <- rbinom(length(p), size = 1, prob=p)
  
  # run the relief algorithm    
  mp <- mungebits:::mungeplane(dataframe)
  mb <- mungebits:::mungebit(relief_alg)
  success <- TRUE
  tryCatch(
    mb$run(mp, threshold=0.1, frac=NA, k=2, verbose=FALSE),
    error = function(e) success <<- FALSE
  )
  
  expect_true(success, info="It didn't crash"")
  
})

