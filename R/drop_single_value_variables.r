#' Drop variables with only one unique value
#'
.drop_single_value_variables <- function(x) {
  if (length(x) == 0 || identical(x, rep(x[[1]], length(x)))) NULL
  else x
}

#' @export
drop_single_value_variables <- column_transformation(.drop_single_value_variables)
