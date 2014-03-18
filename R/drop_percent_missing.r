#' Drop columns with over a certain percent missing.
#'
#' @param x any. The column to screen.
#' @param threshold numeric. The percent (e.g., 0.5) below which to remove
#'   column that have at least that percent missing. The default is 0.8.
#' @export
#' @examples
#' df <- iris; df[, 6] <- NA; df[1, 6] <- 1
#' drop_percent_missing(df, , 0.8) # Drop sixth column
drop_percent_missing <- function(x, threshold = 0.8) {
  if ('drop' %in% names(inputs)) {
    if (inputs$drop) NULL
    else x
  } else {
    if (mean(is.na(x)) < threshold) {
      inputs$drop <<- FALSE
      x
    } else {
      inputs$drop <<- TRUE
      NULL
    }
  }
}
drop_percent_missing <- column_transformation(drop_percent_missing, mutating = TRUE)
