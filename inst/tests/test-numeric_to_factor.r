context("numeric_to_factor helper function")

test_that("it correctly restores a bunch of levels", {
  nums <- -25:25
  levs <- c("[2, 10)", "[10, 20)", "[-5, -3)", "(-20, -10]", "[0, 1]", "-2")
  converts <- numeric_to_factor(nums, levs)

  actual_restores <- 
    c("Missing", "Missing", "Missing", "Missing", "Missing", "Missing",
      "(-20, -10]", "(-20, -10]", "(-20, -10]", "(-20, -10]", "(-20, -10]",
      "(-20, -10]", "(-20, -10]", "(-20, -10]", "(-20, -10]", "(-20, -10]",
      "Missing", "Missing", "Missing", "Missing", "[-5, -3)", "[-5, -3)",
      "Missing", "-2", "Missing", "[0, 1]", "[0, 1]", "[2, 10)", "[2, 10)",
      "[2, 10)", "[2, 10)", "[2, 10)", "[2, 10)", "[2, 10)", "[2, 10)",
      "[10, 20)", "[10, 20)", "[10, 20)", "[10, 20)", "[10, 20)", "[10, 20)",
      "[10, 20)", "[10, 20)", "[10, 20)", "[10, 20)", "Missing", "Missing",
      "Missing", "Missing", "Missing", "Missing")

  expect_equal(converts, actual_restores)
})
