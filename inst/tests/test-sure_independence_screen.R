context("sure_independence_screen")

example <- function(data) {
  df <- data
  df$noise <- pi * seq_len(nrow(df))
  df
}

get.mp <- function(data = iris) {
  df <- example(data)       
  if (identical(data, iris)) { df$dep_var <- as.integer(df$Species == df$Species[1]) }
  attr(df, 'foo') <- 'bar'
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(sure_independence_screen)
  mb$run(mp)
  mp
}

test_that("it stops when a factor would have >100 levels", {
  hundred_handed_one <- data.frame(hands = factor(1:102), dep_var = c(0,1))
  expect_error(get.mp(hundred_handed_one)$data, 'Too many levels') 
})

test_that("it correctly screens out random noise", {
  expect_identical(setdiff(colnames(get.mp()$data), 'dep_var'), colnames(iris)[1:2],
    info = "sure_independence_screen should have filtered noise")
})

test_that("it leaves attributes in place in data", {
  expect_identical(attr(get.mp()$data, 'foo'), 'bar',
    info = "sure_independence_screen should have left the 'foo' attribute in place")
})
