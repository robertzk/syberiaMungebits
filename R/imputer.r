#' Impute a column by mean
#' Only meant to be used as a mungebit
.imputer <- function(x) {
  if (!'mean' %in% inputs) inputs$mean <<- mean(x, na.rm = TRUE)
  x[is.na(x)] <- inputs$mean
  x
}
