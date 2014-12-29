#' Count the occurances of the N most repeated words in a paragraph
#'
#' Paragraphs of M lines for N words will return a data frame of N + 1 columns and M rows each counting the occurance in the particular line.
#'
#' This conducts full word matching only.  Substring matches do not count towards the total.
#'
#' @param input an atomic vector. The paragraph of text to analyse.
#' @return numeric. Number of occurances of the particular word in each line.
#' @examples
#' paragraph <- c("This is an example.", "Great code uses examples like this example.")
#' paramatch(paragraph, 2)


paramatch <- function(input, top_n_words = 5) {
  # Make sure inputs are valid
    stopifnot(is.character(input))
    stopifnot(is.numeric(top_n_words))
    stopifnot(top_n_words >= 1)
    stopifnot(top_n_words %% 1 == 0)
  # Standardize the input
    paragraph <- toupper(input)
    paragraph <- gsub("[[:punct:]]", " ", paragraph)
    paragraph <- gsub("[[:space:]]+", " ", paragraph)
  # Split string into words  
    allwords <- unlist(strsplit(paragraph, " "))
  # Split into unique words
    words <- unique(allwords)
  # Full word matching
    frequency <- data.frame(word = words, 
                            occurances = sapply(words, function(x) length(grep(paste0("\\<",x,"\\>"), allwords))), 
                            stringsAsFactors = FALSE) 
  # Take the top N words
    top_n <- frequency[order(-frequency$occurances),][1:top_n_words,]
  # Create the output data frame
    output <- data.frame(input, sapply(top_n$word, function(x) str_count(paragraph,paste0("\\<",x,"\\>"))))
    colnames(output) <- c("input", sapply(top_n$word, function(x) paste0("col_", x)))

  output
}