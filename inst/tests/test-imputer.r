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
  expect_equal(length(mb$inputs), 2,
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

