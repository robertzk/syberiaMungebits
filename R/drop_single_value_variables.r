drop_single_value_variables <- function(x) {
  if (length(x) == 0 || identical(x, rep(x[[1]], length(x)))) NULL
  else x
}
