#' Create a separate indicator column if the numeric variable has a missing level 

#' @export
missing_indicator <- multi_column_transformation(function(x, impute = TRUE) {
  
  # this is the indicator variable
  z <- as.integer(is.na(x))

  # fill in the missing value in the original numeric variable with the mode (arbitrary)
  if (impute) {
    mode <- x[which.max(table(x))]
    x[is.na(x)] <- mode
  }

  list(x,z)
})
