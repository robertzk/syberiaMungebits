context("drop_single_value_variables_fn")

test_that("it correctly drops a 1-value column", {
  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
  dataframe <- data.frame(cbind(rep(1, 1000), rep(c(1,2), 500)))
  mb <- mungebits:::mungebit(drop_single_value_variables)
  mp <- mungebits:::mungeplane(dataframe)
  mb$run(mp)
  dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  mp2 <- mungebits:::mungeplane(dataframe)
  mb$run(mp2)
  expect_equal(mp$data, mp2$data)
  if (!mungebits_loaded) unloadNamespace('mungebits')
})

test_that("it correctly keeps a 2-value column", {
  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
  dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  mb <- mungebits:::mungebit(drop_single_value_variables)
  mp <- mungebits:::mungeplane(dataframe)
  mb$run(mp)
  expect_equal(mp$data, dataframe)
  dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  mp2 <- mungebits:::mungeplane(dataframe)
  mb$run(mp2)
  expect_equal(mp$data, dataframe)
  if (!mungebits_loaded) unloadNamespace('mungebits')
})


