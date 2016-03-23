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

test_that("it correctly selects rows by function with whole = TRUE", {
  iris2 <- iris
  iris2[1, 1] <- NA
  select_rows(iris2, complete.cases, whole = TRUE)
  expect_equal(iris2, iris[-1, ])
})

test_that("it correctly selects rows by function", {
  dd <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9)) 
  dd2 <- dd
  select_rows(dd, function(x) sum(x) > 15)
  expect_equal(dd, dd2[3, ]) 
})

test_that("it correctly selects rows by function and cols_to_check with whole = FALSE", {
  dd <- data.frame(x = c(1,2,3), y = c(4,5,6), z = c(-1,-2,999))
  dd2 <- dd
  select_rows(dd, function(x) {sum(x) == 0}, whole = FALSE, cols_to_check = c('x', 'z'))
  expect_equal(dd, dd2[1:2, ])
})

test_that("it correctly selects rows by function and cols_to_check with whole = TRUE", {
  dd <- data.frame(x = c(1,2,3), y = c(4,5,6), z = c(-1,-2,999))
  dd2 <- dd
  select_rows(dd, function(x) {(x[[1]] + x[[2]]) > 0}, whole = TRUE, cols_to_check = c('x', 'z'))
  expect_equal(dd, dd2[3, ])
})


