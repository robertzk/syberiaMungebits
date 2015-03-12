#' Drop columns with over a certain percent missing.
#'
#' @param x any. The column to screen.
#' @param threshold numeric. The percent (e.g., 0.5) below which to remove
#'   column that have at least that percent missing. The default is 0.8.
#' @seealso \code{\link{drop_percent_missing}}
#' @examples
#' df <- iris; df[, 6] <- NA; df[1, 6] <- 1; df <- mungebits:::mungeplane(df)
#' mb <- mungebits:::mungebit(drop_percent_missing)
#' mb$run(df, TRUE, 0.8) # Drop sixth column
drop_percent_missing_fn <- function(x, threshold = 0.8) {
  if ('drop' %in% names(inputs)) {
    if (inputs$drop) NULL
    else x
  } else {
    if (mean(is.na(x) | x == 'Missing') < threshold) {
      inputs$drop <<- FALSE
      x
    } else {
      inputs$drop <<- TRUE
      NULL
    }
  }
}

#' Drop columns with over a certain percent missing.
#'
#' @param dataframe a data.frame to drop columns from.
#' @param input_cols a vector of columns to drop percent missing from.
#' @param ... the arguments passed to the transformation (e.g., \code{threshold}).
#' @seealso \code{\link{drop_percent_missing_fn}}
#' @export
drop_percent_missing <- column_transformation(drop_percent_missing_fn, mutating = TRUE)
