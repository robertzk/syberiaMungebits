context('select_variables')

test_that("it correctly selects variables by numeric index", {
  iris2 <- iris
  select_variables(iris2, 1)
  expect_equal(iris2, iris[, 1, drop = FALSE])
})

test_that("it correctly selects variables by logical index", {
  iris2 <- iris
  select_variables(iris2, c(T,F))
  expect_equal(iris2, iris[, c(1,3,5)])
})

test_that("it correctly selects variables by character index", {
  iris2 <- iris
  select_variables(iris2, c('Sepal.Length', 'Sepal.Width'))
  expect_equal(iris2, iris[, 1:2])
})

test_that("it preserves attributes after subsetting", {
  iris2 <- iris
  attr(iris2, 'foo') <- 'bar'
  select_variables(iris2, c('Sepal.Length', 'Sepal.Width'))
  expect_identical(attr(iris2, 'foo'), 'bar')
})


