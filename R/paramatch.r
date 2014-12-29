# Paramunge function - Paraffin 

# The test input
paragraph <- c("The lazy dog is a  dog.","dog jumps fox ,","fox outwits lazy dog","The fox out foxed the dog.","The lazy dog loses because of the lazy dog's lazy eye.")

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

paramatch(paragraph, 3)

# Current Output
#                                                    input col_DOG col_THE col_LAZY col_FOX col_IS
# 1                                The lazy dog is a  dog.       2       1        1       0      1
# 2                                        dog jumps fox ,       1       0        0       1      0
# 3                                   fox outwits lazy dog       1       0        1       1      0
# 4                             The fox out foxed the dog.       1       2        0       1      0
# 5 The lazy dog loses because of the lazy dog's lazy eye.       2       2        3       0      0

# Desired Output
#          input  col_DOG       col_THE       col_LAZY    col_FOX       col_IS
# c(paragraph)    c(2,1,1,1,2)  c(1,0,0,2,2) c(1,0,1,0,3) c(0,1,1,1,0) c(1,0,0,0,0)
