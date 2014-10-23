context("imputer")

setup_imputation_mungebit <- function() {
  iris2 <- iris
  mb <- mungebits:::mungebit(imputer)
  iris2[1, ] <- NA
  iris2 <- mungebits:::mungeplane(iris2)
  mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  list(mb, iris2)
}

medians <- function(dataset) {
  unlist(lapply(dataset[2:150, 1:2], function(x) median(x, na.rm = TRUE)))
}

test_that("it imputes a column in a dataframe correctly", {
  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
  x <- setup_imputation_mungebit()
  mb <- x[[1]]; iris2 <- x[[2]]
  expect_equal(medians(iris), unlist(iris2$data[1, 1:2]))
  # Ignore starred attributes for now
  expect_equal(length(grep("^[^*].*[^*]?", names(mb$inputs))), 2,
    info = paste0("Expecting imputer mungebit to store inputs for 2 columns.",
                  " Did you set ",
                  testthat::colourise("mutating = TRUE", "green"),
                  " when defining the column_transformation?"))
  
  if (!mungebits_loaded) unloadNamespace('mungebits')
})

test_that("it restores an imputed column correctly", {
  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
  . <- setup_imputation_mungebit()
  mb <- .[[1]]; iris2 <- .[[2]]
  iris2$data[1, ] <- NA
  iris2$data <- iris2$data[1, , drop = FALSE]
  mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  # make sure same medians get restored when predicting
  expect_equal(medians(iris), unlist(iris2$data[1, 1:2]),
    info = paste0("The imputer mungebit must be able to restore medians using ",
                  "the trained mungebit"))
  if (!mungebits_loaded) unloadNamespace('mungebits')
})

test_that("it can handle imputation with a function column specifier",  {
  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
 iris2 <- iris
 mb <- mungebits:::mungebit(imputer)
  iris2[1, 1:2] <- NA
  iris2 <- mungebits:::mungeplane(iris2)
  mb$run(iris2, function(x) is.numeric(x) && sum(is.na(x)) > 0)
  iris2$data <- iris
  iris2$data[1, ] <- NA
  out <- tryCatch(mb$run(iris2, function(x) is.numeric(x) && sum(is.na(x)) > 0),
           warning = function(w) "warning")
  expect_identical(out, NULL,  info = "There should not have been any warnings thrown")
  # make sure same medians get restored when predicting
  expect_equal(medians(iris), unlist(iris2$data[1, 1:2]),
    info = paste0("The imputer mungebit must be able to restore medians using ",
                  "the trained mungebit"))
  expect_identical(c(NA_real_, NA_real_), unname(unlist(iris2$data[1, 3:4])),
    info = paste0("The imputer mungebit must not restore inappropriate columns"))

  if (!mungebits_loaded) unloadNamespace('mungebits')
})

test_that("it can impute factors (base case)", {
  
  # make a data.frame
  df <- data.frame(x=1:3, y=factor(c('A','B','B')))
  
  # train it
  mp <- mungebits:::mungeplane(df) 
  mb <- mungebits:::mungebit(imputer)
  mb$run(mp)
  
  # run it on a data.frame with a missing value
  df[1,2] <- NA
  mp2 <- mungebits:::mungeplane(df)
  mb$run(mp2)
  
  # check that it works in the simplest case
  expect_identical(as.character(mp2$data$y), c('B','B','B'), "Failed to impute")
  
})

test_that("for imputing factors it will take the first mode when there are more than one", {
  
  # make a data.frame
  df <- data.frame(x=1:3, y=factor(c('A','B','C')))
  
  # train it
  mp <- mungebits:::mungeplane(df) 
  mb <- mungebits:::mungebit(imputer)
  mb$run(mp)
  
  # run it on a data.frame with a missing value
  df[3,2] <- NA
  mp2 <- mungebits:::mungeplane(df)
  mb$run(mp2)
  
  # check that it works in the simplest case
  expect_identical(as.character(mp2$data$y), c('A','B','A'), "Fails when there are multiple modes")
  
})

test_that("it can impute new levels that the validation data.frame has not seen before", {
  
  # make a data.frame
  df <- data.frame(x=1:3, y=factor(c('A','B','A')))
  
  # train it
  mp <- mungebits:::mungeplane(df) 
  mb <- mungebits:::mungebit(imputer)
  mb$run(mp)
  
  # run it on a data.frame with a missing value
  df <- data.frame(x=1:3, y=factor(c(NA, NA, NA)))
  mp2 <- mungebits:::mungeplane(df)
  mb$run(mp2)
  
  # check that it works in the simplest case
  expect_identical(as.character(mp2$data$y), c('A','A','A'), 
                   "Fails when there is a new level in the factor to be imputed")
  
})


