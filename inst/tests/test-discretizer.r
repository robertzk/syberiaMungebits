# TODO: (RK) Figure out why this is broken on CI
#context("discretizer")
#
#attach(system.file("data/iris_discretized.rda", package = "mungebitsTransformations"))
#
#test_that("it correctly discretizes iris data set", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#  iris2 <- mungebits:::mungeplane(iris)
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(iris2, 1:4, mode_freq_threshold = 0.2, debug = TRUE)
#  expect_equal(iris2$data, iris_discretized,
#    info = paste0("The iris dataset must have been discretized correctly and ",
#                  "match the values in the iris_discretized dataset."))
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
## TODO: (RK) Figure out why this is broken on Travis CI
##test_that("it correctly restores iris data set", {
##  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
##  iris2 <- mungebits:::mungeplane(iris)
##  mb <- mungebits:::mungebit(discretizer)
##  # mode_freq_threshold = 0.15 actually fails to discretize...
##  mb$run(iris2, 1:4, mode_freq_threshold = 0.2, debug = TRUE)
##  # prediction run
##  one_row_discretized <- iris_discretized[1, , drop = FALSE]
##  iris2$data <- iris[1, , drop = FALSE]
##  mb$run(iris2, 1:4, debug = TRUE)
##  expect_equal(iris2$data, one_row_discretized,
##    info = paste0("The discretizer must be able to restore levels using",
##                  "the levels generated during the training run."))
##
##  # for good measure, test multiple row datasets as well
##  ten_rows_discretized <- iris_discretized[1:10, , drop = FALSE]
##  iris2$data <- iris[1:10, , drop = FALSE]
##  mb$run(iris2, 1:4, debug = TRUE)
##  expect_equal(iris2$data, ten_rows_discretized,
##    info = paste0("The discretizer must be able to restore levels using",
##                  "the levels generated during the training run."))
##  if (!mungebits_loaded) unloadNamespace('mungebits')
##})
#
#test_that("it does not discretize values with uniques below the lower bnd", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#  iris2 <- mungebits:::mungeplane(iris)
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(iris2, 1:4, mode_freq_threshold = 0.2,
#         lower_count_bound = 22, debug = TRUE)
#  # Only the fourth column of iris has <= 22 uniques
#  expect_equal(iris2$data[, -4], iris_discretized[, -4]);
#  expect_equal(iris2$data[, 4], iris[, 4])
#
#  # test prediction
#  iris2 <- mungebits:::mungeplane(iris)
#  mb$run(iris2, 1:4)
#  expect_equal(iris2$data[, -4], iris_discretized[, -4]);
#  expect_equal(iris2$data[, 4], iris[, 4])
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
#test_that("it does not discretize values with uniques above the upper bnd", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#  iris2 <- mungebits:::mungeplane(iris)
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(iris2, 1:4, mode_freq_threshold = 0.2,
#         upper_count_bound = 23, debug = TRUE)
#  # Only the fourth column of iris has < 23 uniques
#  expect_equal(iris2$data[, -4], iris[, -4]);
#  expect_equal(iris2$data[, 4], iris_discretized[, 4])
#
#  # test prediction
#  iris2 <- mungebits:::mungeplane(iris)
#  mb$run(iris2, 1:4)
#  expect_equal(iris2$data[, -4], iris[, -4]);
#  expect_equal(iris2$data[, 4], iris_discretized[, 4])
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
#detach(iris_discretized)
#
