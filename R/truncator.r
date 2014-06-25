#' Constrain a numeric column to the range [min,max] of the training set 
#'
#' @param x an atomic vector
#' @param digits how many decimal places you would like your numeric number to contain after its been truncated

truncator_fn <- function(x) {
    
  x <- x[[1]]
  stopifnot(is.numeric(x))
  if (!('min' %in% names(inputs))) {
    inputs$min <<- min(x, na.rm = TRUE)
    inputs$max <<- max(x, na.rm = TRUE)
  } else { 
    
    x[x<=inputs$min] <- syberiaMungebits:::trunc.dig(inputs$min, digits =1)
    x[x>=inputs$max] <- syberiaMungebits:::trunc.dig(inputs$max, digits=1)
  }
  x
}

# Truncates Decimals after specified numers of Digits. 
#' i.e trunc.dig(5.732 , digits = 1) => 5.7 

trunc.dig <- function(x, digits = 1) trunc(x*10^digits)/10^digits

#' @export
truncator <- column_transformation(truncator_fn, mutating = TRUE, named = TRUE)

