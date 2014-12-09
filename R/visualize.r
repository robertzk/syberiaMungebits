#' attain visualize insights into data  

visualize <- function(df, ...) {
  par(mfrow = c(2,2)) 
  for (i in 1:ncol(df) ) {
    if ( i %% 4 == 0 ) par(ask=TRUE) 
    overlayed_histogram(df, hist_var = names(df)[i], ...)
  }
  par(ask = FALSE) 
}

#' @export
# visualize <- column_transformation(visualize, mutating = FALSE, whole = TRUE)

