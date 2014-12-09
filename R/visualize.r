#' attain visualize insights into data  

visualize <- function(df) {
  for (i in 1:ncol(df) {
    overlayed_histogram(df, df[,i], histColor = 'brown', lineColor = 'blue', bins = 12)       
  }
}

# Truncates Decimals after specified numers of Digits. 
#' i.e trunc.dig(5.732 , digits = 1) => 5.7 
# trunc.dig <- function(x, digits = 1) trunc(x * 10^digits) / 10^digits

#' @export
# truncator <- column_transformation(truncator_fn, mutating = TRUE, named = TRUE)

