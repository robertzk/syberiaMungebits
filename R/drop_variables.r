#' Drop variables in a dataframe.
#'
#' This is a column transformation and will be wrapped in
#' \code{column_transformation}.
#'
#' @param x an atomic vector. The column to process.
#' @return NULL ensures the column gets removed
#' @examples
#' drop_variables(iris, 1) # Drop first column
#' drop_variables(iris, c('Sepal.Length', 'Petal.Length'))
#' drop_variables(iris, c(T,T,F,F,T)) # only leave columns 3 and 4
.drop_variables <- function(x) NULL

#' @export
drop_variables <- column_transformation(.drop_variables)
