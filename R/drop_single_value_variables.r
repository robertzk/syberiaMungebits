#' Drop variables with only one unique value
#' @param x an atomic vector.
drop_single_value_variables_fn <- function(x) {
  if (length(x) == 0 || identical(x, rep(x[[1]], length(x)))) NULL
  else x
}

#' @export
drop_single_value_variables <-
  column_transformation(drop_single_value_variables_fn)
