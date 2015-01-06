#' Mungebit to count the occurances of the N most repeated words in a dataframe column.
#'
#' Trains to find the most repeated words.  Then counts the number of matches for dataframes passed to it.
#'
#' This conducts full word matching only.  Substring matches do not count towards the total.
#'
#' @param dataframe
#' @param col character. The column name pertaining to paragraphs for matching.
#' @param top_n_words integer.  The number of top strings to count in each line.
#' @param suppress.input logical.  Suppresses the Input column of the output data frame.
#' @param blacklist character. Prevents undesired words participating int he output.
#' @return numeric. Number of occurances of the particular word in each line.
#' @author Mike Bruno
#' @examples
#' paragraph <- data.frame(id = c(1:2), text = c("This is an example.", "Great code uses examples like this example."), stringsAsFactors = FALSE)
#' paramatch(paragraph, "text", 2)

#' @export
paramatch <- function(dataframe, col, top_n_words = 5, suppress.input = FALSE, blacklist = c()) {
  # Grab the string vector
  paragraph_col <- dataframe[[col]]
  # Make sure inputs are valid
  stopifnot(is.character(paragraph_col))
  stopifnot(is.numeric(top_n_words))
  stopifnot(top_n_words >= 1)
  stopifnot(top_n_words %% 1 == 0)
  stopifnot(is.logical(suppress.input))
  # Standardize the input
  paragraph_col <- toupper(paragraph_col)
  paragraph_col <- gsub("[[:punct:]]", " ", paragraph_col)
  paragraph_col <- gsub("[[:space:]]+", " ", paragraph_col)
  # Munge
  if(!('top_n' %in% names(inputs))) { # Train # TODO add something in case top N length is differnt from inputs
    # Find the top N words
    # Split string into words  
    allwords <- unlist(strsplit(paragraph_col, " "))
    allwords <- allwords[allwords != ""]
    # Split into unique words
    words <- unique(allwords)
    # Full word matching
    frequency <- data.frame(word = words, 
                            occurances = sapply(words, function(x) length(grep(paste0("\\<", x, "\\>"), allwords))), 
                            stringsAsFactors = FALSE) 
    # Blacklist
    frequency <- frequency[!frequency$word %in% blacklist, ]
    # Ensure that N isn't greater than the total number of unique words d
    if(length(allwords) < top_n_words) {
      message("Note: N is too high, defaulting to the number of unique words")
      top_n_words <- length(words)
    }
    # Take the top N words
    top_n <- frequency[order(frequency$occurances, decreasing = TRUE),][1:top_n_words, 1]
    # Write top_n to the Environment
    inputs$top_n <<- top_n
  } else {
    top_n <- inputs$top_n
  }

  # Run the count for the top n words
  output <- data.frame(sapply(top_n, function(x) stringr::str_count(paragraph_col, paste0("\\<", x, "\\>"))))
  colnames(output) <- c(sapply(top_n, function(x) paste0("col_", x)))
  # Create the final dataframe with additional columns
  eval(substitute({
    dataframe <- cbind(dataframe, output)
    # Optional - Suppress Input - hide the original paragraph column
    if (suppress.input) dataframe[[col]] <- NULL
  }), envir = parent.frame()
  )
}
