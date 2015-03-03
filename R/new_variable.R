#' Creates a new variable in the dataframe.
#' @param df dataframe. The dataframe to modify.
#' @param fun function. The function to run to convert inputs to outputs.
#' @param inputs character. The names of the variables needed to make the output.
#' @param output character. The name of the desired new variable.
#' @export
new_variable <- function(df, fun, output, inputs = NULL) {
  if (is.null(inputs)) inputs <- as.character(names(formals(fun)))
  if (!is.function(fun)) {
    f <- function() {}
    body(f) <- fun
    fun <- f
  }
  eval.parent(substitute({
    multi_column_transformation(fun)(df, inputs, output)
    dataframe <- df
  }))
}
