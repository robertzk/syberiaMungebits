#' Sure independence screening function
#'
#' @param dataframe a data.frame. Run sure independence screening on this
#'   dataframe.
#' @param ... further SIS options. See also \code{sure_independence_screening} in
#'   the \code{statsUtils} package.
#' @importFrom statsUtils sure_independence_screening
#' @export
sure_independence_screen <- function(dataframe, ...) {
  browser()
  eval(substitute({
    column_transformation(function(column, ...)
      statsUtils::sure_independence_screening(dataframe$dep_var, column, ...)
    )(dataframe, ...)
  }), envir = parent.frame())
}
