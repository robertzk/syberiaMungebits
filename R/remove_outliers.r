#' remove outliers based upon a z_score threshold z_score=> (x - mean(x) / sd(x) 
#'
#' @param x an numeric vector
#' @param threshold a z_score threshold from which the absolute value of a z_score above this threshold will be set to NA 

remove_outliers_fn <- function(x , threshold ) {
    x[abs(as.vector(scale(x))) > threshold] <- NA
}

#' @export
remove_outliers <- column_transformation(remove_outliers_fn, mutating = TRUE, named = TRUE)

