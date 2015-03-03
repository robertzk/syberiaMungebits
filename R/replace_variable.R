#' Replaces a variable in the dataframe.
#' @param df dataframe. The dataframe to modify.
#' @param fun function. The function to run to convert the input into the new input.
#' @export
replace_variable <- function(df, fun) {
  eval.parent(substitute({
    new_variable(df, fun, as.character(names(formals(fun))[[1]]))
    dataframe <- df
  }))
}
