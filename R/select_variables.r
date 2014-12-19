#' Select variables in a dataframe.
#'
#' @param dataframe a data.frame
#' @param cols an atomic vector. Drop all but these columns.
#' @param weak logical. Whether or not to add non-existent columns as NAs.
#' @export
#' @examples
#' df <- iris; select_variables(df, 1) # Select only first variable
#' df <- iris; select_variables(df, c('Sepal.Length', 'Petal.Length'))
#' df <- iris; select_variables(df, c(TRUE,TRUE,FALSE,FALSE,TRUE)) # Exclude cols 3 and 4
select_variables <- function(dataframe, cols, weak = TRUE) {
  cols <- mungebits:::standard_column_format(cols, dataframe)
  if (weak) {
    na_cols <- setdiff(cols, colnames(dataframe))
    cols <- intersect(colnames(dataframe), cols)
  }
  eval(substitute({
    for (col in setdiff(colnames(dataframe), cols)) {
      dataframe[[col]] <- NULL
    }

    if (weak && length(na_cols) > 0) dataframe[, na_cols] <- NA
  }), envir = parent.frame())
}

