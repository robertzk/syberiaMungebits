context("discretizer")
require(mungebits)
# TODO: (RK) Figure out why this is broken on CI

#test_that("it correctly discretizes iris data set", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#  iris2 <- mungebits:::mungeplane(iris)
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(iris2, 1:4, mode_freq_threshold = 0.2)
#  expect_equal(iris2$data, iris_discretized,
#    info = paste0("The iris dataset must have been discretized correctly and ",
#                  "match the values in the iris_discretized dataset."))
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
#test_that("it correctly restores iris data set", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#  iris2 <- mungebits:::mungeplane(iris)
#  mb <- mungebits:::mungebit(discretizer)
#  # mode_freq_threshold = 0.15 actually fails to discretize...
#  mb$run(iris2, 1:4, mode_freq_threshold = 0.2)
#  # prediction run
#  one_row_discretized <- iris_discretized[1, , drop = FALSE]
#  iris2$data <- iris[1, , drop = FALSE]
#  mb$run(iris2, 1:4)
#  expect_equal(iris2$data, one_row_discretized,
#    info = paste0("The discretizer must be able to restore levels using",
#                  "the levels generated during the training run."))
#
#  # for good measure, test multiple row datasets as well
#  ten_rows_discretized <- iris_discretized[1:10, , drop = FALSE]
#  iris2$data <- iris[1:10, , drop = FALSE]
#  mb$run(iris2, 1:4)
#  expect_equal(iris2$data, ten_rows_discretized,
#    info = paste0("The discretizer must be able to restore levels using",
#                  "the levels generated during the training run."))
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
#test_that("it does not discretize values with uniques below the lower bnd", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#  iris2 <- mungebits:::mungeplane(iris)
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(iris2, 1:4, mode_freq_threshold = 0.2,
#         lower_count_bound = 22)
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
#         upper_count_bound = 23)
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

test_that("it correctly uses the missing_level argument", {
          
  df <- mungebits:::mungeplane(data.frame(first = 1:100)); df$data[1, 1] <- NA
  mb <- mungebits:::mungebit(discretizer)
  mb$run(df, 1, missing_level = "Not here")

  expected_discretized_column <-
    factor(c('Not here', rep('[ 2, 35)', 33), rep('[35, 68)', 33), rep('[68,100]', 33)))
  expect_equal(df$data[[1]], expected_discretized_column)

  # test prediction
  df <- mungebits:::mungeplane(data.frame(first = 1:50)); df$data[1, 1] <- NA
  mb$run(df, 1, missing_level = "Not here")
  expect_equal(df$data[[1]], expected_discretized_column[1:50])
})
#
#test_that("it correctly uses the missing_level argument if it is NULL", {
#  df <- mungebits:::mungeplane(data.frame(first = 1:100)); df$data[1, 1] <- NA
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(df, 1, missing_level = NULL)
#
#  expected_discretized_column <-
#    factor(c(NA, rep('[ 2, 35)', 33), rep('[35, 68)', 33), rep('[68,100]', 33)))
#  expect_equal(df$data[[1]], expected_discretized_column)
#
#  # test prediction
#  df <- mungebits:::mungeplane(data.frame(first = 1:50)); df$data[1, 1] <- NA
#  mb$run(df, 1, missing_level = NULL)
#  expect_equal(df$data[[1]], expected_discretized_column[1:50])
#})
#
#test_that("it leaves values that are not observed in the train set as NA", {
#  df <- mungebits:::mungeplane(data.frame(first = 1:100))
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(df, 1)
#  # test prediction
#  expected_discretized_column <-
#    factor(c(rep('[ 1, 35)', 34), rep('[35, 68)', 33), rep('[68,100]', 33)))
#  df <- mungebits:::mungeplane(data.frame(first = 1:150))
#  mb$run(df, 1)
#  expect_equal(df$data[[1]],
#    factor(c(as.character(expected_discretized_column), rep(NA, 50)),
#           levels = levels(df$data[[1]])))
#})
#
#
#test_that("it supports up to 10 digits in discretization levels", {
#  df <- mungebits:::mungeplane(data.frame(first = 1:100 / 4000000))
#  mb <- mungebits:::mungebit(discretizer)
#  mb$run(df, 1)
#  expected_discretized_column <-
#    factor(c(rep("[0.00000025,0.00000875)", 34), rep("[0.00000875,0.00001700)", 33),
#             rep("[0.00001700,0.00002500]", 33)))
#  expect_equal(df$data[[1]], expected_discretized_column)
#
#  # test prediction
#  df <- mungebits:::mungeplane(data.frame(first = (1:100 / 4000000)[1:50]))
#  mb$run(df, 1)
#  expect_equal(df$data[[1]], expected_discretized_column[1:50])
#})
#
#
#test_that("Discretizer Successfully Interacts with Truncators to Truncate Extreme values ", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#
#  xmin = -83.892
#  xmax = 16.108
#  
#  x.train <- seq(xmin,xmax,length.out=100) 
#  df1 <- data.frame(x=x.train)
#  mp1 <- mungebits:::mungeplane(df1)
#  
#  x.predict <- c(xmin,mean(c(xmin,xmax)),xmax)
#  df2 <- data.frame(x=x.predict)
#  mp2 <- mungebits:::mungeplane(df2)
#    
#  mbTrunc <- mungebits:::mungebit(truncator)
#  mbDisc <- mungebits:::mungebit(discretizer)
#  
#  mbTrunc$run(mp1)
#  mbDisc$run(mp1, 1, lower_count_bound=0)
#  
#  
#  mbTrunc$run(mp2)
#  mbDisc$run(mp2, 1, lower_count_bound=0)
#  
#
#
#  # check that none of the outputs are missing in discretizer
#  expect_equal(sum(mp2$data[,1]=="Missing"),0)
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
#test_that("Discretizer can handle unforseen Factor Levels", {
#  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
#
#  xmin = -83.892
#  xmax = 16.108
#  
#  x.train <- seq(xmin,xmax,length.out=100) 
#  df1 <- data.frame(x=x.train)
#  mp1 <- mungebits:::mungeplane(df1)
#  
#  x.predict <- c(xmin,mean(c(xmin,xmax)),xmax)
#  df2 <- data.frame(x=x.predict)
#  mp2 <- mungebits:::mungeplane(df2)
#    
#  mbDisc <- mungebits:::mungebit(discretizer)
#  
#  mbDisc$run(mp1, 1, lower_count_bound=0)
#  mbDisc$run(mp2, 1, lower_count_bound=0)
#  
#
#
#  # check that none of the outputs are missing in discretizer
#  expect_equal(sum(mp2$data[,1]=="Missing"),0)
#  if (!mungebits_loaded) unloadNamespace('mungebits')
#})
#
#
## test_that("it successfully bins values on the boundary of the training phase", {
#
##test_that("Within Discretizer It Doesent Contain Factor Gaps", {
##  
##  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
##  
##  iris2 <- mungebits:::mungeplane(iris)
##  iris2$data[100:nrow(iris2$data),1]<-0 
##   
##
##  mb <- mungebits:::mungebit(discretizer)
##
##  mb$run(iris2, 1, mode_freq_threshold = .1,granularity = 4)
##   
##  # test prediction
##  iris2 <- mungebits:::mungeplane(iris)
##  mb$run(iris2, 1:4)
##  expect_equal(iris2$data[, -4], iris[, -4]);
##  expect_equal(iris2$data[, 4], iris_discretized[, 4])
##  
##  if (!mungebits_loaded) unloadNamespace('mungebits')
##})
#  
