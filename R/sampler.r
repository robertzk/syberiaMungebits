#' Take a data frame and upsample a single segment
#' i.e. Given a variable var and a value val, randomly sample with replacement the rows with var == val
#'
#' @param df a data frame
#' @export
sampler <- function(df, var, val, frac) {

  if (("trained" %in% names(inputs))) return(invisible(NULL)) 
  inputs$trained <<- TRUE

  eval(substitute({
    rows <- which(df[[var]]==val) 
    n <- length(rows)
    n2 <- round(frac*n)
    rows_to_keep = sample(rows, n2, replace=T)
    the_rows <- df[rows_to_keep,]
    the_rest <- df[df[[var]]!=val,]
    df <- rbind(the_rows, the_rest)
  }), envir = parent.frame())
}
