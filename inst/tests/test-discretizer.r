context("discretizer")

test_that("it correctly discretizes iris data set", {
  iris2 <- mungebits:::mungeplane(iris)
  mb <- mungebits:::mungebit(discretizer)
  mb$run(iris2, 1:4, mode_freq_threshold = 0.2)
  expect_equal(iris2$data, iris_discretized,
    info = paste0("The iris dataset must have been discretized correctly and ",
                  "match the values in the iris_discretized dataset."))
})

