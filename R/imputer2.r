#' Imputer2
#'
#' Impute columns by fitting models to other columns in data set
#' Numeric variables are imputed by a LASSO regression, while
#' factor variables are imputed using LDA.  The imputation works
#' only in training mode because it is difficult to guarantee that 
#' an arbitrary test set will be correctly predicted by the lars
#' and lda functions.  (For example, if there are new levels
#' of a factor variable in the test set, the model matrix during
#' prediction will have a different number of columns than during 
#' training, causing the lars algorithm to fail.)
#'
#' @param dep_var_name Name of the dependent variable.  (Default is "dep_var".)
#' @param maxlevels Maximum number of levels allowed for a factor variable to be imputed.  Too many levels would cause the mungebit to be very slow.

#' @export
imputer2 <- function(dataframe) {
  
  sink('~/dev/null')
  library(lars, quietly = TRUE)
  library(MASS, quietly = TRUE)
  sink()
  
  # get parameters
  trained <- inputs$trained %||% FALSE
  dep_var_name <- inputs$dep_var_name %||% "dep_var"
  maxlevels <- inputs$maxlevels %||% 30
  
  if (trained) {
    cat("Imputer2 applies only during training\n")
    return(dataframe)
  }
  
  # which columns to use for imputation
  impute_cols <- setdiff(names(dataframe), dep_var_name)
  
  # do the imputation and save models used for imputing
  inputs$models <<- list()
    
  # loop over columns to be imputed
  new_columns <- list()
  for (col in impute_cols) {
    
    cat('              Imputing ',col,'\n',sep='')

    # get the response
    y <- dataframe[[col]]
    if (!any(is.na(y))) next # no need to do imputation
    if (all(is.na(y) | y=='')) {
      warning(paste0(col," has no nonmissing values for imputation"))
      next
    }
    if (length(y)==0) {
      warning(paste0(col, " has zero length"))
      next
    }
    if (length(table(y))==1) {
      warning(paste0(col, " has only a single nonmissing value"))
      next
    }
    
    # quick imputation of remaining variables
    vars <- setdiff(names(dataframe), c(dep_var_name, col))
    df <- dataframe[, vars]
    badvars <- c()
    for (cc in names(df)) {
      
      # retrieve this column
      x <- df[[cc]]
      x[x==''] <- NA
      
      # drop bad variables; imput otherwise
      if (length(x)==0) {
        badvars <- c(badvars, cc)
        next
      } else if (all(is.na(x) | x=='')) {
        badvars <- c(badvars, cc)
        next
      } else if (length(table(x))==1) {
        badvars <- c(badvars, cc)
        next
      } else if (is.factor(x) || is.character(x)) {
        tt <- table(x)
        if (length(tt) > 10) {
          badvars <- c(badvars, cc)
          next
        } else {
          mode <- names(which.max(tt))
          x[is.na(x)] <- mode
        }
      } else if (is.numeric(x)) {
        x[is.na(x)] <- mean(x, na.rm=TRUE)
      }
      df[[cc]] <- x
      
    }
    
    # get rid of the bad variables
    keep.vars <- setdiff(names(df), badvars)
    df <- subset(df, select=keep.vars)
    
    # drop records associated with missing response
    which.missing <- is.na(y)
    y.sub <- y[!which.missing]
    
    if (is.numeric(y)) { # lasso regression
      
      # make model matrices
      x <- model.matrix(formula('~.'), data=df)
      x.sub <- x[!which.missing,]
      
      # Fit with LASSO penalty
      success <- TRUE
      tryCatch({
        my_cv <- cv.lars(x.sub, y.sub, type='lasso', plot.it=FALSE) # find MSE along lasso path
        frac1se <- which(my_cv$cv < min(my_cv$cv + my_cv$cv.error))[1]/100 # best lambda within 1 s.e. of minimum   
        my_model <- lars(x.sub, y.sub, type = 'lasso') # fit a lars model object
      }, error = function(e) {
        warning(paste0("Failed to impute ",col))
        success <<- FALSE
      })
      if (!success) next
      
      # get fitted values for all records
      preds <- predict.lars(my_model, x, s=frac1se, type="fit", mode="fraction")$fit # use the model to predict 
        
    } else if (length(table(y)) <= maxlevels) { # lda
      
      # subset to only non-factor columns
      factor.cols <- suppressWarnings(
        apply(df, 2, function(x) any(is.na(as.numeric(as.character(x))))))
      df.sub <- subset(df, subset=!which.missing, select=!factor.cols)
      df.pred <- subset(df, select=!factor.cols)
      
      # do the lda
      success <- TRUE
      tryCatch({
        my_lda <- lda(x=df.sub, grouping=y.sub) # fit an lda
      }, error = function(e) {
        warning(paste0("Failed to impute ",col),'\n')
        success <<- FALSE
      })
      if (!success) next
      
      # predict on all records
      dimen = min(ncol(df.sub), 5) # number of lda dimensions to use
      preds <- predict(my_lda, df.pred, dimen=dimen)$class
  
    }
    
    # get the new column
    new_column <- dataframe[[col]] # original column, missings and all
    new_column[is.na(new_column)] <- preds[is.na(new_column)] # imputation step
    new_columns[[col]] <- new_column # add to list
  }
  
  # replace columns with their new imputed versions
  for (col in names(new_columns)) {
    dataframe[[col]] <- new_columns[[col]] 
  }
  
  inputs$trained <<- TRUE
}
