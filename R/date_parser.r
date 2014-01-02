#' Parse string dates into POSIXct
#'
#' This is a column transformation and will be wrapped in
#' \code{column_transformation}.
#'
#' @param col an atomic vector. The column to process.
#' @return numeric. Number of years
#' @examples
#' dd <- data.frame(cbind(c("", "8 days", "20 days")), stringsAsFactors = FALSE)
#' date_parser(dd, 1)
#' print(dd[[1]])
#' # [1] 0 8 20
.date_parser <- function(col) {
  vapply(col, function(string) {
    as.numeric(
      substring(string, 1,
        max(which(strsplit(string, '')[[1]] == ' ')[1] - 1, 0, na.rm = TRUE)
      )
    )
  }, numeric(1), USE.NAMES = FALSE)
}

#' @export
date_parser <- column_transformation(.date_parser)

