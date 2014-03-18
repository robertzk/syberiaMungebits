context("drop_percent_missing")

setup_mungebit <- function(num = 60) {
  eval.parent(substitute({
    mb <- mungebits:::mungebit(drop_percent_missing)
    df <- iris
    df[, 6] <- NA
    df[seq_len(num), 6] <- 1
    mp <- mungebits:::mungeplane(df)
    mb$run(mp, TRUE, 0.6)
  }))
}

test_that("it drops a column with 60% missing", {
  setup_mungebit()
  expect_true(mean(is.na(df[[6]])) >= 0.6)
  expect_identical(mp$data, iris[, 1:5])
})

test_that("it does not drop a column with less than 60% missing", {
  setup_mungebit(61)
  expect_true(mean(is.na(df[[6]])) < 0.6)
  expect_identical(mp$data, df)
})

