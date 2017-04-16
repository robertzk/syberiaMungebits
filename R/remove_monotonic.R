#' Drop one of each monotonically related pair
#'
#' @param dataframe a data.frame.  Numeric columns must already be set to numeric with 
#'   the appropriate mungebit.  (Non-numeric columns will not be considered).  Also only
#'   only complete cases will be considered, so imputation should already have occurred.
#' @param threshold numeric. If the Spearman rank correlation exceeds this value, columns will be
#'   dropped.
#' @param verbose logical.  Whether to print extra info.
#' @export
remove_monotonic <- function(dataframe, threshold, verbose=FALSE) {

  if (!'drop_columns' %in% names(inputs)) {
    
    # subset to numeric columns only
    numeric_cols <- unlist(lapply(dataframe, is.numeric))
    numeric_cols <- dataframe[, numeric_cols]
    
    # simple imputation
    for (i in 1:ncol(numeric_cols)) {
      x <- numeric_cols[,i]
      x[is.na(x)] <- median(x, na.rm=TRUE)
      numeric_cols[,i]<- x
    }
    
    # keep complete cases only
    numeric_cols <- numeric_cols[complete.cases(numeric_cols), ]
    
    # get spearman correlation coefficients
    corr_matrix <- cor(numeric_cols, method='spearman')

    # find variables greater than threshold
    drop_columns <- c()
    ncols <- ncol(corr_matrix)
    varnames <- names(numeric_cols)
    for (i in 1:(ncols-1)) {
      for (j in (i+1):ncols) {
        if (abs(corr_matrix[i,j]) > threshold) {
          drop_columns <- append(drop_columns, varnames[j])
          if (verbose) {
            cat("Dropping ", varnames[j], " due to correlation with ",
                varnames[i], ' (corr=', formatC(corr_matrix[i,j], digits=3),
                ')\n', sep='')
          }
        }
      }
    }
    inputs$drop_columns <<- unique(drop_columns)
  }
  
  drop_columns <- inputs$drop_columns
  eval.parent(substitute(for (varname in drop_columns) dataframe[[varname]] <- NULL))
  
  TRUE
}
 
