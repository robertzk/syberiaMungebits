#' Restores categorical variables, allowing you to predict.
#' @param x. The column to restore.
#' @export
restore_categorical_variables_fn <- function(x) {
  if ('levels' %in% names(inputs)) {
    factor(x, levels = inputs$levels)
  } else {
    inputs$levels <<- levels(x); x
  }
}

#' @export
restore_categorical_variables <- column_transformation(syberiaMungebits:::restore_categorical_variables_fn, mutating = TRUE)
