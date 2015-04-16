#' Select rows in a dataframe.
#'
#' @param dataframe a data.frame
#' @param rows an atomic vector or function. Drop all but these rows.
#'    If \code{rows} is a function, the rows will be selected based on the
#'    \code{whole} parameter. If \code{whole = TRUE}, the whole dataframe
#'    will be passed in, and the resulting row indices (character, numeric,
#'    or logical) will be selected. If \code{whole = FALSE} (the default)
#'    the function will be applied to each row and the result will be expected
#'    to be a logical, with only the rows returning \code{TRUE} being selected.
#' @param whole a logical. See the \code{rows} parameter. The default is 
#'    \code{FALSE}.
#' @param ... additional arguments to \code{rows} is that parameter is a function. 
#' @export
#' @examples
#' select_rows(iris, 1:10) # Select only first ten rows
#' select_rows(iris, c(TRUE,FALSE)) # Select only odd rows
#' iris2 <- iris; rownames(iris2) <- paste0("row", 1:nrow(iris2))
#' select_rows(iris, c("row10", "row51")) # Select rows by name
select_rows <- function(dataframe, rows, whole = FALSE, ...) {
  force(rows); force(whole); args <- force(list(...))
  eval(substitute({
    dataframe <- dataframe[if(is.function(rows)) {
      if (whole) rows(dataframe) else apply(dataframe, 1, rows)
    } else rows, ]
  }), envir = parent.frame())
}

