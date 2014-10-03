#' Drop one of each monotonically related pair
#'
#' @param dataframe A data.frame.
#' @param depvarname string Name of the dependent variable
#' @param threshold double Score above which to keep variables.
#' @param frac double Fraction of data to use for estimating predictor scores.  If NA, then use all rows.  If NULL, then min(100, nrow(dataframe)) rows are chosen at random.
#' @param verbose logical Whether to output information.
#' @export
relief_alg <- function(dataframe, depvarname='dep_var', frac=NULL,
                       threshold, verbose=FALSE) {

  #if (!'drop_columns' %in% names(inputs)) {
    
    # Extract the dependent variable
    if (!depvarname %in% names(dataframe)) stop(paste0(depvarname, ' is not in dataframe'))
    response <- dataframe[[depvarname]]
    dataframe[[depvarname]] <- NULL
    
    # Drop records with missings
    keep <- complete.cases(dataframe)
    dataframe <- dataframe[keep, ]
    if (verbose) cat("Dropping ", sum(!keep), " rows with NAs\n", sep='')
    if (nrow(dataframe)==0) stop("No rows in dataframe")
    
    # Drop factors
    keep <- !unlist(lapply(dataframe, is.factor))
    dataframe <- dataframe[ , keep]
    if (verbose) cat("Dropping ", sum(!keep), " factor columns\n", sep='')
    if (ncol(dataframe)==0) stop("No columns in dataframe")
    
    # Select random rows for measuring variable importance
    if (is.null(frac)) {
      n.rows <- min(100, nrow(dataframe))
    } else if (is.na(frac)) {
      n.rows <- nrow(dataframe)  
    } else {
      if (!is.double(frac)) stop("frac must be a double")
      if (frac<=0 || frac>=1) stop("frac must be between 0 and 1")
      n.rows <- ceiling(frac*nrow(dataframe))
    }
    rows <- sample.int(nrow(dataframe), n.rows)
    if (verbose) cat("Computing variable importances on ", n.rows, " rows\n", sep='')
    
    # Subset data and standardize columns
    dataframe <- dataframe[rows, ]
    dataframe <- as.data.frame(lapply(dataframe, scale))
    response <- response[rows]
    if (sum(response==0)<2) stop("Not enough 0s in data")
    if (sum(response==1)<2) stop("Not enough 1s in data")
    if (length(unique(response))>2) stop("Response variable has more than two levels")
    
    # Initialize scores
    scores <- rep(0, ncol(dataframe))
    
    # Loop over these records
    for (i in seq_len(nrow(dataframe))) {
      
      # Retrieve the record and associated class
      record <- dataframe[i, ]
      my_class <- response[i]
      
      # Compute distances to other records (Euclidean)
      distances <- apply(dataframe, 1, function(r) sum((record - r)^2))
      
      # Find closest hit and closest miss
      hits <- setdiff(which(response==my_class), i)
      misses <- which(response!=my_class)
      closest.hit <- hits[which.min(distances[hits])]
      closest.miss <- misses[which.min(distances[misses])]
      
      # Update score vector
      scores <- (scores
                 -(record - dataframe[closest.hit, ])^2 / ncol(dataframe) 
                 +(record - dataframe[closest.miss, ])^2 / ncol(dataframe))
      
    #}
    
    # inputs$drop_columns <<- unique(drop_columns)
  }
  
  print(scores)
  
  # drop_columns <- inputs$drop_columns
  # eval.parent(substitute(for (varname in drop_columns) dataframe[[varname]] <- NULL))
  
  TRUE
}

dataframe <- iris
for (i in 1:4) {
  p <- 1/(1 + exp(-3*scale(dataframe[,i])))
  dataframe$dep_var <- rbinom(length(p), size = 1, prob=p)
  relief_alg(dataframe, frac=NA, verbose=TRUE)
}



