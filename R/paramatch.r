#' Identify and Count occurances of the most repeated words.
#'
#' Finds the N the most repeated words in a character column of a dataframe and appends N columns containing the number of matches of each word in each row.
#'
#' This conducts full word matching only.  Substring matches do not count towards the total.
#'
#' @param dataframe
#' @param col character. The column name pertaining to paragraphs for matching.
#' @param top_n integer.  The number of top strings to count in each line.
#' @param suppress.input logical.  Suppresses the Input column of the output data frame.
#' @param blacklist character. Prevents undesired words participating int he output.
#' @return numeric. Number of occurances of the particular word in each line.
#' @author Mike Bruno
#' @examples
#' mp <- mungebits::mungeplane(data.frame(id = c(1:2), text = c("This is an example.", "Great code uses examples like this example."), stringsAsFactors = FALSE))
#' mb <- mungebits:::mungebit(paramatch)
#' mb$run(mp, col = 'text', top_n = 2)
#' @export
paramatch <- function(dataframe, col, top_n = 5, suppress.input = FALSE, blacklist = c()) {
  # Grab the string vector
  paragraph_col <- dataframe[[col]]
  # Make sure inputs are valid
  stopifnot(is.character(paragraph_col))
  stopifnot(is.integer(top_n))
  stopifnot(top_n >= 1)
  stopifnot(is.logical(suppress.input))
  # Standardize the input
  paragraph_col <- toupper(paragraph_col)
  paragraph_col <- gsub("[[:punct:]]", " ", paragraph_col)
  paragraph_col <- gsub("[[:space:]]+", " ", paragraph_col)
  # Munge
  
  if(!(exists('inputs') && paste0(col, "_top_n_words") %in% names(inputs))) { # Train
    # Find the top N words
    # Split string into words
    allwords <- unlist(strsplit(paragraph_col, " "))
    allwords <- allwords[allwords != ""]
    # Find the unique
    words <- unique(allwords)
    # Full word matching
    frequency <- data.frame(word = words,
                            occurances = unlist(lapply(words, function(x) length(grep(paste0("\\<", x, "\\>"), allwords)))),
                            stringsAsFactors = FALSE)
    # Blacklist
    frequency <- frequency[!frequency$word %in% blacklist, ]
    # Ensure that N isn't greater than the total number of unique words
    if(length(allwords) < top_n) {
      message("Note: N is too high, defaulting to the number of unique words")
      top_n <- length(words)
    }
    # Take the top N words
    top_n_words <- frequency[order(frequency$occurances, decreasing = TRUE),][1:top_n, 1]
    # Write col_name_top_n_words to the Environment
    inputs[[paste0(col, "_top_n_words")]] <<- top_n_words
    } else { # Predict
      top_n_words <- inputs[[paste0(col, "_top_n_words")]]
  }
  # Add a column of match counts for each of the top n words
  for (i in 1:length(top_n_words)){
    output <- data.frame(stringr::str_count(paragraph_col, paste0('\\<', top_n_words[i], '\\>')))
    colnames(output) <- paste0("col_", top_n_words[i])
    #Add the Column to the exterior dataset
    eval(substitute({
      dataframe <- cbind(dataframe, output)
    }), envir = parent.frame()
    )
  }
  # Set the added column names and optionally suppress the input column
  if(suppress.input) eval(substitute({
    dataframe[[col]] <- NULL
  }), envir = parent.frame()
  )
}
