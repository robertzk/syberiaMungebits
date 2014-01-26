context('select_rows')

test_that("it correctly selects rows by numeric index", {
  iris2 <- iris
  select_rows(iris2, 1:10)
  expect_equal(iris2, iris[1:10, ])
})

test_that("it correctly selects rows by logical index", {
  iris2 <- iris
  select_rows(iris2, c(T,F))
  expect_equal(iris2, iris[c(T,F), ])
})

test_that("it correctly selects rows by character index", {
  iris2 <- iris
  select_rows(iris2, c('Sepal.Length', 'Sepal.Width'))
  expect_equal(iris2, iris[c('Sepal.Length', 'Sepal.Width'), ])
})

test_that("it correctly selects rows by character index", {
  iris2 <- iris
  iris2[1, 1] <- NA
  select_rows(iris2, complete.cases)
  expect_equal(iris2, iris[-1, ])
})

