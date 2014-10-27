context("remove_outliers")

# mungebit tests
mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); suppressMessages(require(mungebits))

test_that("removes eggregious outliers ", {
  vec = rnorm(20)
  vec[10] = 10 
  df <- data.frame(vec) 
  mb <- mungebits:::mungebit(remove_outliers)
  mp <-mungebits:::mungeplane(df)
  mb$run(mp) 
  expect_that(is.na(mp$data[10, ]), equals(TRUE))
})

test_that("works with negative values ", {
  vec = rnorm(20)
  vec[10] = -10 
  df <- data.frame(vec) 
  mb <- mungebits:::mungebit(remove_outliers)
  mp <-mungebits:::mungeplane(df)
  mb$run(mp) 
  expect_that(is.na(mp$data[10, ]), equals(TRUE))
})

test_that("will remove outliers based upon TRAIN mean & sd ", {
  vec = rnorm(20)
  vec[10] = 10 
  df <- data.frame(vec) 
  mb <- mungebits:::mungebit(remove_outliers)
  mp <-mungebits:::mungeplane(df)
  mb$run(mp) 
  mp2 <-mungebits:::mungeplane(data.frame(vec = c(33, 19, 44, 1)))
  mb$run(mp2) 
  expect_that(sum(is.na(mp2$data[, 1])), equals(3)) # all but last value is an outlier
})

test_that("threshold argument works ", {
  vec = rnorm(20)
  vec[10] = -10 
  df <- data.frame(vec) 
  mb <- mungebits:::mungebit(remove_outliers)
  mp <-mungebits:::mungeplane(df)
  mb$run(mp, threshold = 0 )  # will remove all values as the absolute value of all z-scores > 0 
  expect_that(all(is.na(mp$data[, 1])), equals(TRUE))
})


run_mungebit <- function(runner) {
  iris2 <- mungebits:::mungeplane(iris)
  mb <- mungebits:::mungebit(renamer)
  mb$run(iris2, runner)
  iris2
}

if (!mungebits_loaded) unloadNamespace('mungebits')
