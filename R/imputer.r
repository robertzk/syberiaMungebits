#' Impute a column by mean
#' Only meant to be used as a mungebit
#'
#' @param x an atomic vector.
imputer_fn <- function(x) {
  if (!'replacement' %in% names(inputs)) {
    if (is.numeric(x)) {
      inputs$replacement <<- median(x, na.rm=TRUE)
    } else {
      tt <- table(x)
      inputs$replacement <<- names(tt)[which.max(tt)]
    }
    if (length(inputs$replacement)==0) inputs$replacement <<- NA
  }
  if (is.factor(x)) levels(x) <- union(levels(x), inputs$replacement)
  x[is.na(x)] <- inputs$replacement
  x
}

#' @export
imputer <- column_transformation(imputer_fn, mutating = TRUE)

