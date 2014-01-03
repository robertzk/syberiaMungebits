context("imputer")

setup_imputation_mungebit <- function() {
  require("mungebits")
  iris2 <- iris
  mb <- mungebits:::mungebit(imputer)
  iris2[1, ] <- NA
  iris2 <- mungebits:::mungeplane(iris2)
  mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  list(mb, iris2)
}

means <- function(dataset) {
  unlist(lapply(dataset[2:150, 1:2], function(x) mean(x, na.rm = TRUE)))
}

test_that("it imputes a column in a dataframe correctly", {
  x <- setup_imputation_mungebit()
  mb <- x[[1]]; iris2 <- x[[2]]
  expect_equal(means(iris), unlist(iris2$data[1, 1:2]))
  expect_equal(length(mb$inputs), 2,
    info = paste0("Expecting imputer mungebit to store inputs for 2 columns.",
                  " Did you set ",
                  testthat::colourise("mutating = TRUE", "green"),
                  " when defining the column_transformation?"))
  
})

test_that("it restores an imputed column correctly", {
  . <- setup_imputation_mungebit()
  mb <- .[[1]]; iris2 <- .[[2]]
  iris2$data[1, ] <- NA
  iris2$data <- iris2$data[1, , drop = FALSE]
  mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  # make sure same means get restored when predicting
  expect_equal(means(iris), unlist(iris2$data[1, 1:2]),
    info = paste0("The imputer mungebit must be able to restore means using ",
                  "the trained mungebit"))
})

