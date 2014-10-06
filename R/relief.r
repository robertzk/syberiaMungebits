#' Drop unimportant variables.
#'
#' @param dataframe A data.frame.
#' @param depvarname string Name of the dependent variable
#' @param threshold double Score above which to keep variables.
#' @param frac double Fraction of data to use for estimating predictor scores.  If NA, then use all rows.  If NULL, then min(100, nrow(dataframe)) rows are chosen at random.
#' @param k integer Number of nearest neighbors to use.  Higher values should improve accuracy but slow the algorithm down.
#' @param verbose logical Whether to output information.
#' @export
relief_alg <- function(dataframe, depvarname='dep_var', frac=NULL,
                       k=1, threshold, verbose=FALSE) {

  #if (!'drop_columns' %in% names(inputs)) {
    
    # Extract the dependent variable
    if (!depvarname %in% names(dataframe)) stop(paste0(depvarname, ' is not in dataframe'))
    response <- dataframe[[depvarname]]
    dataframe[[depvarname]] <- NULL
    
    # Drop records with missings
    #keep <- complete.cases(dataframe)
    #dataframe <- dataframe[keep, ]
    #if (verbose) cat("Dropping ", sum(!keep), " rows with NAs\n", sep='')
    #if (nrow(dataframe)==0) stop("No rows in dataframe")
    
    # Drop factors
    #keep <- !unlist(lapply(dataframe, is.factor))
    #dataframe <- dataframe[ , keep]
    #if (verbose) cat("Dropping ", sum(!keep), " factor columns\n", sep='')
    #if (ncol(dataframe)==0) stop("No columns in dataframe")
    
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
    dataframe <- dataframe[rows, ]
    response <- response[rows]

    # Convert to model.matrix
    #orig.col.names <- colnames(dataframe)
    #mm <- model.matrix(~.-1, dataframe)
    #dataframe <- as.data.frame(mm)
    
    # Standardize numeric columns
    #dataframe <- as.data.frame(lapply(dataframe, scale))
    for (n in 1:ncol(dataframe)) {
      x <- dataframe[ ,n]
      if (is.numeric(x)) dataframe[ ,n] <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
    }
    if (sum(response==0)<2) stop("Not enough 0s in data")
    if (sum(response==1)<2) stop("Not enough 1s in data")
    if (length(unique(response))>2) stop("Response variable has more than two levels")
    
    # which columns are factors
    factor.cols <- sapply(dataframe, function(x) is.factor(x) || is.character(x))
    
    # basic difference between two records
    n.levels <- apply(dataframe, 2, function(x) length(table(x)))
    delta <- function(r1, r2) {
      
      r1.n <- as.numeric(r1[!factor.cols])
      r2.n <- as.numeric(r2[!factor.cols])
      r1.f <- r1[factor.cols]
      r2.f <- r2[factor.cols]
      
      # difference of numeric part
      results.n <- abs(r1.n - r2.n)
      
      # difference of factor part
      results.f <- ifelse(r1.f==r2.f, 0, 1)
      
      # combine parts
      results <- r1
      results[!factor.cols] <- results.n
      results[factor.cols] <- results.f
      
      # fill in missings with RELIEF-B method
      results[is.na(results)] <- 1 - 1/n.levels[is.na(results)]
      
      # return results
      results
    }
    
    # range of variable (1 for factors)
    range2 <- function(x) if (is.numeric(x)) range(x, na.rm=T) else c(0,1)
    df.range <- sapply(dataframe, range2)
    df.range <- df.range[2,] - df.range[1,]
    
    # difference function (normalized by range of variable)
    difference <- function(r1, r2) {
      delta(r1,r2)/df.range
    }
  
    # function to find the smallest n things
    which.min.n <- function(x,n) {
      order(x)[1:min(n, length(x))]
    }
    
    # Initialize scores
    scores <- rep(0, ncol(dataframe))
    
    # Loop over these records
    for (i in seq_len(nrow(dataframe))) {
      
      # Retrieve the record and associated class
      record <- dataframe[i, ]
      my_class <- response[i]
      
      # Compute distances to other records (Euclidean)
      distances <- apply(dataframe, 1, function(r) sum(delta(record,r)^2))
      
      # Find closest hit and closest miss
      hits <- setdiff(which(response==my_class), i)
      misses <- which(response!=my_class)
      closest.hit.indices <- hits[which.min.n(distances[hits], k)]
      closest.miss.indices <- misses[which.min.n(distances[misses], k)]
      closest.hits <- dataframe[closest.hit.indices, ]
      closest.misses <- dataframe[closest.miss.indices, ]
      
      # Update score vector
      for (row in 1:nrow(closest.hits)) {
        hit <- as.numeric(closest.hits[row, ])
        scores <- scores - difference(record, hit) / nrow(dataframe) / k
      }
      for (row in 1:nrow(closest.misses)) {
        miss <- as.numeric(closest.misses[row, ])
        scores <- scores + difference(record, miss) / nrow(dataframe) / k
      }
      
    #}
    
    # inputs$drop_columns <<- unique(drop_columns)
  }
  
  # output the final scores
  print(scores)
  
  # drop_columns <- inputs$drop_columns
  # eval.parent(substitute(for (varname in drop_columns) dataframe[[varname]] <- NULL))
  
  TRUE
}



dataframe <- iris
for (i in 1:5) {
  if (i==5) {
    p <- ifelse(dataframe$Species=='setosa', 0.8, 0.2)
  } else {
    p <- 1/(1 + exp(-5*scale(dataframe[,i])))
  }
  dataframe$dep_var <- rbinom(length(p), size = 1, prob=p)
  relief_alg(dataframe, frac=NA, k=10, verbose=TRUE)
}





dataframe <- list()
complete.df <- list()
for (i in 1:5) {
  
  varname <- paste0('var',i)
  
  # 4 numeric columns and 1 factor column
  if (i==5) {
    dataframe[[varname]] <- sample(LETTERS[1:5], size=200, replace=TRUE)
  } else {
    dataframe[[varname]] <- rnorm(200)
  }
  
  # original dataframe without missing values
  complete.df[[varname]] <- dataframe[[varname]]
  
  # make some rows missing
  na.rows <- sample.int(n=200, size=20)
  dataframe[[varname]][na.rows] <- NA 
}
complete.df <- as.data.frame(complete.df)
dataframe <- as.data.frame(dataframe)

for (i in 1:5) {
  if (i==5) {
    p <- ifelse(complete.df$var5=='C', 0.99, 0.01)
  } else {
    p <- 1/(1 + exp(-5*scale(complete.df[,i])))
  }
  dataframe$dep_var <- rbinom(length(p), size = 1, prob=p)
  relief_alg(dataframe, frac=NA, k=10, verbose=TRUE)
}





