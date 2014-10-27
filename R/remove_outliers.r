#' remove outliers based upon a z_score threshold z_score=> (x - mean(x) / sd(x) 
#'
#' @param x an numeric vector
#' @param threshold a z_score threshold from which the absolute value of a z_score above this threshold will be set to NA 

remove_outliers_fn <- function(x, threshold ) {
    if (!'removed_outliers' %in% names(inputs)) {
      inputs$mean <- mean(x, is.na=T)
      inputs$sd <- sd(x, is.na=T) 
      inputs$removed_outliers <- TRUE  ## not sure if another mungebit has a mean namespace 
    }
    x[abs((x - inputs$mean(x)) / inputs$sd) > threshold] <- NA
    x
}

#' @export
remove_outliers <- column_transformation(remove_outliers_fn )

