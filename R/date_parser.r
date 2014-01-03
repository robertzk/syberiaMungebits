#' Parse numeric date distances into numerics.
#'
#' For example, c("8 days", "100 days", "") will become c(8, 100, NA)
#'
#' This is a column transformation and will be wrapped in
#' \code{column_transformation}.
#'
#' @param column an atomic vector. The column to process.
#' @return numeric. Number of years
#' @examples
#' dd <- data.frame(cbind(c("", "8 days", "20 days")), stringsAsFactors = FALSE)
#' date_parser(dd, 1)
#' print(dd[[1]])
#' # [1] 0 8 20
date_parser_fn <- function(column) {
  suppressWarnings(as.numeric(sub(" .*", "", column)))
}

#' @export
date_parser <- column_transformation(date_parser_fn)

