#' Order a dataframe by a column
#' @export
orderer <- function(dataframe, column_name) {
  eval(substitute(dataframe <- dataframe[order(dataframe[[column_name]]), ]),
       envir = parent.frame())
}
