#' Creates a new variable in the dataframe.
#' @param df dataframe. The dataframe to modify.
#' @param fun function. The function to run to convert old_variables to new_variables.
#' @param old_variables character. The names of the variables needed to make the new_variable.
#' @param new_variable character. The name of the desired new variable.
#' @export
new_variable <- function(df, fun, new_variable, old_variables = NULL) {
  if (is.null(old_variables)) old_variables <- as.character(names(formals(fun)))
  if (!is.function(fun)) {
    f <- function() {}
    body(f) <- fun
    fun <- f
  }
  eval.parent(substitute({
    multi_column_transformation(fun)(df, old_variables, new_variable)
    dataframe <- df
  }))
}
