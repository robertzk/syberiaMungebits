#' Constrain a numeric column to the range [min,max] of the training set 
#'
#' @param x an atomic vector
truncator_fn <- function(x) {
  if (!('min' %in% names(inputs))) {
    inputs$min <<- min(x, na.rm = TRUE)
    inputs$max <<- max(x, na.rm = TRUE)
  } else { 
    x[x<inputs$min] <- inputs$min
    x[x>inputs$max] <- inputs$max
  }
  x
}

#' @export
truncator <- column_transformation(truncator_fn, mutating = TRUE)

