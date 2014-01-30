context("value_replacer_fn")

test_that("it replaces unnamed values correctly", {
  column <- c("A", "B", NA, "D")
  column <- value_replacer_fn(column, list(list(c("A", "B", "D"), 1), list(NA, 0)))
  expect_equal(column, c("1", "1", "0", "1"))
})

test_that("it replaces named values correctly", {
  column <- c("A", "B", NA, "D")
  column <- value_replacer_fn(column, list(A = 1, B = 2, D = 3))
  expect_equal(column, c("1", "2", NA, "3"))
})

test_that("it replaces mixed values correctly", {
  column <- c("A", "B", NA, "D")
  column <- value_replacer_fn(column, list(A = 1, B = 2, D = 3, list(NA, 0)))
  expect_equal(column, c("1", "2", "0", "3"))
})

test_that("it replaces in-place correctly", {
  column <- c(0, 1, 0, 1)
  column <- value_replacer_fn(column, list(c(0, 1), c(1, 0)))
  expect_equal(column, c(1, 0, 1, 0))
})

test_that("it replaces numerics correctly", {
  column <- c(0, 1, 2, 3)
  column <- value_replacer_fn(column, list(list(c(0, 1), 0), list(c(2, 3), 1)))
  expect_equal(column, c(0, 0, 1, 1))
})

test_that("it replaces factors correctly", {
  column <- factor(c("a", "b", "c"))
  column <- value_replacer_fn(column, list(a = "b", b = "a", c = "d"))
  expect_true(identical(column,
                        factor(c("b", "a", "d"), levels = c("b", "a", "d"))))
})

# mungebit tests

mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); suppressMessages(require(mungebits))

test_that("it restores unnamed values correctly", {
  column <- c("A", "B", NA, "D")
  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  mb <- mungebits:::mungebit(value_replacer)
  mb$run(testdf, 1, list(list(c("A", "B", "D"), 1), list(NA, 0)))
  expect_equal(testdf$data[[1]], c("1", "1", "0", "1"))
})

test_that("it restores named values correctly", {
  column <- c("A", "B", NA, "D")
  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  mb <- mungebits:::mungebit(value_replacer)
  mb$run(testdf, 1, list(A = 1, B = 2, D = 3))
  expect_equal(testdf$data[[1]], c("1", "2", NA, "3"))
})

test_that("it restores mixed values correctly", {
  column <- c("A", "B", NA, "D")
  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  mb <- mungebits:::mungebit(value_replacer)
  mb$run(testdf, 1, list(A = 1, B = 2, D = 3, list(NA, 0)))
  expect_equal(testdf$data[[1]], c("1", "2", "0", "3"))
})

test_that("it restores in-place correctly", {
  column <- c(0, 1, 0, 1)
  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  mb <- mungebits:::mungebit(value_replacer)
  mb$run(testdf, 1, list(c(0, 1), c(1, 0)))
  expect_equal(testdf$data[[1]], c(1, 0, 1, 0))
})

test_that("it restores numerics correctly", {
  column <- c(0, 1, 2, 3)
  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  mb <- mungebits:::mungebit(value_replacer)
  mb$run(testdf, 1, list(list(c(0, 1), 0), list(c(2, 3), 1)))
  expect_equal(testdf$data[[1]], c(0, 0, 1, 1))
})

test_that("it restores factors correctly", {
  column <- factor(c("a", "b", "c"))

  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  mb <- mungebits:::mungebit(value_replacer)
  mb$run(testdf, 1, list(a = "b", b = "a", c = "d"))
  expect_equal(class(testdf$data[[1]]), 'factor')
  expect_true(identical(testdf$data[[1]],
                        factor(c("b", "a", "d"), levels = c("b", "a", "d"))))

  # Have to test predicts as well to make sure levels are restored
  testdf <- mungebits:::mungeplane(data.frame(column, column, stringsAsFactors = FALSE))
  testdf$data <- testdf$data[1, ]
  mb$run(testdf, 1, list(a = "b", b = "a", c = "d"))
  expect_equal(class(testdf$data[[1]]), 'factor')
  expect_true(identical(testdf$data[1, 1],
                        factor('b', levels = c("b", "a", "d"))))
})

if (!mungebits_loaded) unloadNamespace('mungebits')

