#' Select rows in a dataframe.
#'
#' @param dataframe a data.frame
#' @param rows an atomic vector. Drop all but these rows.
#' @export
#' @examples
#' select_rows(iris, 1:10) # Select only first ten rows
#' select_rows(iris, c(T,F)) # Select only odd rows
#' iris2 <- iris; rownames(iris2) <- paste0("row", 1:nrow(iris2))
#' select_rows(iris, c("row10", "row51") # Select rows by name
select_rows <- function(dataframe, rows, whole = FALSE) {
  force(rows)
  eval(substitute({
    dataframe <- dataframe[if(is.function(rows)) {
      if (whole) rows(dataframe) else apply(dataframe, 1, rows)
    } else rows, ]
  }), envir = parent.frame())
}

