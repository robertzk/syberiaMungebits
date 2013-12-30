#' Parse string dates into POSIXct
#'
#' This is a column transformation and will be wrapped in
#' \code{column_transformation}.
#'
#' @param col an atomic vector. The column to process.
#' @return numeric. Number of years
#' @export
#' @importFrom stringr str_extract
date_parser <- function(col) {
  as.numeric(str_extract(col, '^[0-9]+'))
}

