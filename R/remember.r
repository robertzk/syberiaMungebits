#' remembers information as of a given step in the mungeprocedure
#'
#' @param f function of the form f(dataframe) that returns the statistic of interest, e.g. f <- function(x) nrows(x)
#' @param key binding for the statistic of interest
#' 
#' @details the result of apply f to dataframe is stored in a list called "memory" in the base environment
#' @examples
#' remember(df,dim,"dimensions")
#' > memory
#' $dimensions
#' [1] 454 463

#' @export
remember <- function(dataframe,f,key) {

  if (!("trained" %in% names(inputs))) { 
    # Write it to the base environment
    # This is so wrong, but hey, it works
    env <- as.environment("package:base")
    if (!is.null(env$memory)) env$memory <- list()
    env$memory[[key]] <- f(dataframe) 
    inputs$trained <<- TRUE
  }

  invisible(NULL)
}
