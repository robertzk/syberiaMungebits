#' Attain Visual insights into data. Plot overlayed histogram vs dep_var for each variable in dataframe
#'
#' @param dataframe a data.frame
#' @param ... excess arguments one can pass to the overlayed_histogram function. I.E. line_var, histColor, etc.
#' @export
visualize  <- function(dataframe, ...) {
  par(mfrow = c(2,2)) # can make it a param if requested
  for (i in 1:ncol(dataframe) ) {
    if ( i %% 4 == 0 ) par(ask=TRUE) 
    overlayed_histogram(dataframe, hist_var = names(dataframe)[i], ...)
  }
  # TODO -> how to force code below to run with interrupts 
  par(ask = FALSE) 
  par(mfrow = c(1,1))
}

