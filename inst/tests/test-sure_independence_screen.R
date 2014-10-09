context("sure_independence_screen")

example <- function() {
  df <- iris
  df$noise <- pi * seq_len(nrow(df))
  df$dep_var <- as.integer(df$Species == df$Species[1])
  df
}

get.mp <- function () {
  df <- example()       
  attr(df, 'foo') <- 'bar'
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(sure_independence_screen)
  mb$run(mp)
  mp
}

test_that("it correctly screens out random noise", {
  expect_identical(setdiff(colnames(get.mp()$data), 'dep_var'), colnames(iris)[1:2],
    info = "sure_independence_screen should have filtered noise")
})

test_that("it leaves attributes in place in data", {
  expect_identical(attr(get.mp()$data, 'foo'), 'bar',
    info = "sure_independence_screen should have left the 'foo' attribute in place")
})

