context("imputer")

test_that("it imputes a column in a dataframe correctly", {
  require("mungebits")
  iris2 <- iris
  mb <- mungebits:::mungebit(imputer)
  iris2[1, ] <- NA
  iris2 <- mungebits:::mungeplane(iris2)
  mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  means <- unlist(lapply(iris[2:150, 1:2], function(x) mean(x, na.rm = TRUE)))
  expect_equal(means, unlist(iris2$data[1, 1:2]))
  expect_equal(length(mb$inputs), 2,
    info = paste0("Expecting imputer mungebit to store inputs for 2 columns.",
                  " Did you set ",
                  testthat::colourise("mutating = TRUE", "green"),
                  " when defining the column_transformation?"))
  
  iris2$data[1, ] <- NA
  iris2$data <- iris2$data[1, , drop = FALSE]
  mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  # make sure same means get restored when predicting
  expect_equal(means, unlist(iris2$data[1, 1:2]),
    info = paste0("The imputer mungebit must be able to restore means using ",
                  "the trained mungebit"))
})

