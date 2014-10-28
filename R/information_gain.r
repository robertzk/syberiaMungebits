#' Select topN features based on theoretical information gain
#'
#' @param dataframe data.frame It is what you think it is.
#' @param depvar string Name of the 0/1 response variable
#' @param topN integer Number of variables to keep
#' @param min.category integer Numeric variables are discretized in bins, with the bin size chosen so that on average each bin should have at least min.category 0s and min.category 1s.

entropy <- function(y) {
  N0 <- sum(y==0)
  N1 <- sum(y==1)
  N <- length(y)
  term1 <- ifelse(N1==0, 0, N1*log2(N1/N))
  term0 <- ifelse(N0==0, 0, N0*log2(N0/N))
  (term1 + term0)/N
}

information_gain <- function(x, y, min.category=10) {
  
  # convert dates to numbers
  if (inherits(x,'Date')) x <- as.integer(x)
  
  # convert numbers to factors by discretizing
  if (is.numeric(x)) {
    response.ratio <- min(mean(y), 1-mean(y))
    min.bucket.size <- min.category/response.ratio
    num.buckets <- floor(length(x)/min.bucket.size)
    if (num.buckets < 2) {
      warning('Variable is too small to be discretized')
      return(-Inf)
    }
    probs <- seq(0, 1, length.out=num.buckets+1)
    probs <- probs[-c(1,length(probs))]
    qq <- c(-Inf, quantile(x, probs=probs), Inf)
    x <- factor(cut(x, qq))
  }
  
  # compute the importance metric
  ig <- 0
  for (v in unique(x)) {
    y_sub <- y[x==v]
    ig <- ig + length(y_sub)*entropy(y_sub)
  }
  -ig
}

#' @export
infogain <- function(dataframe, depvar='dep_var', topN=50, min.category=10) {
  
  # training phase
  if (!'drop_columns' %in% names(inputs)) {
    
    # loop over columns of dataframe
    inputs$ig_scores <<- list()
    for (var in setdiff(names(dataframe), depvar)) {
      x <- dataframe[[var]]
      y <- dataframe[[depvar]]
      ig <- information_gain(x, y, min.category)
      inputs$ig_scores[[var]] <<- ig
    }
    
    # create the list of columns to be dropped
    scores <- unlist(inputs$ig_scores)
    keep_columns <- names(inputs$ig_scores)[order(scores)][seq_len(min(length(scores),topN))]
    inputs$drop_columns <<- setdiff(names(dataframe), c(depvar, keep_columns))
  }
  
  # drop all but the topN columns
  drop_columns <- inputs$drop_columns
  eval.parent(substitute(for (varname in drop_columns) dataframe[[varname]] <- NULL))
  
  TRUE
}

