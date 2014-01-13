#' Sure independence screening function
#'
#' @param dataframe a data.frame. Run sure independence screening on this
#'   dataframe.
#' @param ... further SIS options. See also \code{sure_independence_screening} in
#'   the \code{statsUtils} package.
#' @importFrom statsUtils sure_independence_screening
#' @export
sure_independence_screen <- function(dataframe, ...) {
  eval(substitute({
    column_transformation(function(column, ...) {
      if (is.null(statsUtils::sure_independence_screening(dataframe$dep_var, column, ...)))
        NULL
      else column
    })(dataframe, ...)
  }, list(dataframe = substitute(dataframe))), envir = parent.frame())
}
