context('orderer')

test_that("it orders a dataframe correctly", {
  iris2 <- iris
  orderer(iris2, 'Sepal.Length')
  expect_equal(iris2, iris[order(iris$Sepal.Length), ])
})
