#' Discretizer
#'
#' @importFrom arules discretize
#' @importFrom stringr str_trim
#' @importFrom Ramd pp
.discretizer <- function(column,
    granularity = 3, mode_freq_threshold = 0.15, mode_ratio_threshold = 1.5,
    category_range = min(granularity, 20):20) {

  mode_value <- Mode(column)
  if (mean(column == mode_value, na.rm = TRUE) > mode_freq_threshold &&
      mode_ratio(column) > mode_ratio_threshold) {
    mode_corrected <- FALSE
    if (!is.null(category_range)) {
      for(i in category_range) {
        discretized_column <- try(discretize(column,
                                             method = 'frequency',
                                             categories = i))
        if (inherits(discretized_column, 'try-error')) next 
        trimmed_levels <- str_trim(levels(discretized_column))
        if (mode_value %in% supressWarnings(as.numeric(trimmed_levels))) {
          mode_corrected <- TRUE
          break
        }
      }
      if (!mode_corrected) {
        # TODO: Turn into binary variable
        # binary_variables <<- append(binary_variables, colname)

        stop(pp("Mode of variable '#{colname}' is above #{100 * mode_freq_threshold}% ",
                "and/or mode ratio is above #{mode_ratio_threshold} and no number of buckets between ",
                "#{min(category_range)} and #{max(category_range)} fixes the problem. May want to ",
                "discretize manually")) 
      }
    }
  } else {
    discretized_column <- try(discretize(column,
                                     method = 'frequency',
                                     categories = granularity))
  }

  if (inherits(discretized_column, 'try-error'))
    stop(pp("Problem discretizing variable '#{colname}': #{discretized_column}"))
  else discretized_column
}

#' @export
discretizer <- column_transformation(discretizer, mutating = TRUE)

# Some helper functions
mode_ratio <- function(variable) {
  freqs = tabulate(match(variable, unique(variable)))
  freqs[order(-freqs)[1]] / freqs[order(-freqs)[2]]
}

# http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
Mode <- function(variable) {
  uniques <- unique(variable[!is.na(variable)])
  uniques[which.max(tabulate(match(variable, uniques)))]
}

