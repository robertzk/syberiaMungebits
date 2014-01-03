context("discretizer")

test_that("it correctly discretizes iris data set", {
  iris2 <- mungebits:::mungeplane(iris)
  mb <- mungebits:::mungebit(discretizer)
  mb$run(iris2, 1:4, mode_freq_threshold = 0.2, debug = TRUE)
  expect_equal(iris2$data, iris_discretized,
    info = paste0("The iris dataset must have been discretized correctly and ",
                  "match the values in the iris_discretized dataset."))
})

test_that("it correctly restores iris data set", {
  iris2 <- mungebits:::mungeplane(iris)
  mb <- mungebits:::mungebit(discretizer)
  # mode_freq_threshold = 0.15 actually fails to discretize...
  mb$run(iris2, 1:4, mode_freq_threshold = 0.2, debug = TRUE)
  # prediction run
  one_row_discretized <- iris_discretized[1, , drop = FALSE]
  iris2$data <- iris[1, , drop = FALSE]
  mb$run(iris2, 1:4, debug = TRUE)
  expect_equal(iris2$data, one_row_discretized,
    info = paste0("The discretizer must be able to restore levels using",
                  "the levels generated during the training run."))

  # for good measure, test multiple row datasets as well
  ten_rows_discretized <- iris_discretized[1:10, , drop = FALSE]
  iris2$data <- iris[1:10, , drop = FALSE]
  mb$run(iris2, 1:4, debug = TRUE)
  expect_equal(iris2$data, ten_rows_discretized,
    info = paste0("The discretizer must be able to restore levels using",
                  "the levels generated during the training run."))
})

