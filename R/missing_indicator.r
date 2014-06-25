#' Create a separate indicator column if the numeric variable has a missing level 

#' @export
missing_indicator <- multi_column_transformation(function(x) {
  
  # this is the indicator variable
  z <- rep(0,length(x))
  z[is.na(x)] <- 1

  # fill in the missing value in the original numeric variable with the mode (arbitrary)
  mode <- x[which.max(table(x))]
  x[is.na(x)] <- mode

  list(x,z,NULL)
})
