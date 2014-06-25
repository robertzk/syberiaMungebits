#' Drop variables with only one unique value
#' @param x an atomic vector.
drop_single_value_variables_fn <- function(x) {
  if ('dropped' %in% names(inputs)) {
    if (inputs$dropped) return(NULL)
    else return(x)
  }

  if (length(x) == 0 || (tmp <- length(unique(x))) == 1 ||
      (tmp == 2 && any(is.na(x)))) {
    inputs$dropped <<- TRUE
    NULL
  } else {
    inputs$dropped <<- FALSE
    x 
  }
}

#' @export
drop_single_value_variables <-
  column_transformation(drop_single_value_variables_fn, mutating = TRUE)
