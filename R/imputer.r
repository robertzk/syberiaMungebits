#' Impute a column by mean
#' Only meant to be used as a mungebit
#'
#' @param x an atomic vector.
imputer_fn <- function(x) {
  if (!'median' %in% names(inputs)) inputs$median <<- median(x, na.rm = TRUE)
  x[is.na(x)] <- inputs$median
  x
}

#' @export
imputer <- column_transformation(imputer_fn, mutating = TRUE)

