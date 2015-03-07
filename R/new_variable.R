#' Creates a new variable in the dataframe.
#'
#' Wrapper around the default multi_column_transformation mungebit to make new variables.
#'
#' @param df dataframe. The dataframe to modify.
#' @param fun function. The function to run to convert old_variables to new_variables.
#'        Also can take a non-function expression that will be converted to function() expression.
#' @param old_variables character. The names of the variables needed to make the new_variable.
#' @param new_variable character. The name of the desired new variable.
#' @return Does not return anything, but modifies-in-place the dataframe to have that new variable.
#' @author Peter Hurford
#' @examples
#' new_variable(iris, function(Sepal.Width, Sepal.Length) Sepal.Width*2 + Sepal.Length*2, 'Sepal.Perimeter')
#' new_variable(iris, iris$Sepal.Width, 'Sepal.Width2')
#'
#' Usage in a Syberia file:
#' list("Make new variable" = list(
#'   new_variable,
#'   function(Sepal.Width, Sepal.Length) Sepal.Width*2 + Sepal.Length*2,
#'   'Sepal.Perimeter'
#' ))
#'
#' @export
new_variable <- function(df, fun, new_variable, old_variables = NULL) {
  if (is.null(old_variables)) old_variables <- as.character(names(formals(fun)))
  if (!is.function(fun)) {
    f <- function() {}
    body(f) <- fun
    fun <- f
  }
  eval.parent(substitute({
    mungebits::multi_column_transformation(fun)(df, old_variables, new_variable)
    dataframe <- df
  }))
}
