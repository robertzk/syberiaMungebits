context("restore_categorical_variables")

test_that('it can restore levels correctly in prediction', {
  mb <- mungebits:::mungebit(restore_categorical_variables)
  mp <- mungeplane(iris)
  mb$run(mp)
  mp <- mungeplane(data.frame(Species = 'setosa'))
  mb$run(mp)
  expect_identical(levels(mp$data$Species), levels(iris$Species))
})
