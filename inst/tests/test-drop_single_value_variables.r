context("drop_single_value_variables_fn")

run_test <- function(switch=0) {
  mungebits_loaded <- 'mungebits' %in% loadedNamespaces(); require(mungebits)
  if (switch == 0) { dataframe <- data.frame(cbind(rep(1, 1000), rep(c(1,2), 500))) }
  else { dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500))) }
  mb <- mungebits:::mungebit(drop_single_value_variables)
  mp <- mungebits:::mungeplane(dataframe)
  mb$run(mp)
  if (switch == 1) { expect_equal(mp$data, dataframe) }
  dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  mp2 <- mungebits:::mungeplane(dataframe)
  mb$run(mp2)
  if (switch == 0) { expect_equal(mp$data, mp2$data) }
  if (!mungebits_loaded) unloadNamespace('mungebits')
}
test_that("it correctly drops a 1-value column", run_test(0))
test_that("it correctly keeps a 2-value column", run_test(1))
