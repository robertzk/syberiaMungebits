#' Impute a column by mean
#' Only meant to be used as a mungebit
#'
#' @param x an atomic vector.
imputer_fn <- function(x) {
  if (!'mean' %in% names(inputs)) inputs$mean <<- mean(x, na.rm = TRUE)
  x[is.na(x)] <- inputs$mean
  x
}

#' @export
imputer <- column_transformation(imputer_fn, mutating = TRUE)

