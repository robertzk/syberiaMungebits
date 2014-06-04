#' Rename columns in a dataframe
#'
#' @param dataframe a data.frame.
#' @param replacements a list of name replacements with keys old names and
#'    respective values new names.
#' @export
#' @examples
#' renamer(iris, list('Sepal.Length' = 'seplen', 'Sepal.Width' = 'sepwid'))
renamer <- function(dataframe, replacements) {
  eval(substitute({
    colnames(dataframe) <-
      Reduce(function(str, old_value) {
        str[colnames(dataframe) == old_value] <- replacements[[old_value]]
        str
      }, names(replacements), colnames(dataframe))
  }), envir = parent.frame())
}
