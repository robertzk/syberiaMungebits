#' Order a dataframe by a column
#' @param dataframe a data.frame to re-order
#' @param column_name the column by which to order
#' @export
orderer <- function(dataframe, column_name) {
  eval(substitute(dataframe <- dataframe[order(dataframe[[column_name]]), ]),
       envir = parent.frame())
}
