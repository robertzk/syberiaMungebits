#' Discretizer function
#'
#' @param column an atomic vector. The variable to discretize.
#' @param granularity an integer. The suggested number of levels.
#' @param mode_freq_threshold a real value between 0 and 1. If the mode of the
#'    variable exceeds this value and is greater than
#'    \code{mode_ratio_threshold} (see next parameter) times the next greatest
#'    mode (i.e., the ratio of the value occuring most often over the value
#'    occuring second most often is over \code{mode_ratio_threshold}) then
#'    the variable will be attempted to be discretized in manner as to make
#'    the mode its own bucket. (so if the mode is 5, we'd want, e.g., [2,4),
#'    5, and (5, 7]).
#' @param mode_ratio_threshold a real value. See the \code{mode_freq_threshold}
#'    parameter.
#' @param category_range The number of levels to consider when the
#'    discretization procedure descrized in the \code{mode_freq_threshold}
#'    parameter is employed. The default is \code{min(granularity, 20):20}.
#' @param lower_count_bound an integer. Variables with less than or equal to
#'    this many unique values will not get discretized. Default is
#'    \code{granularity}.
#' @param upper_count_bound an integer. Variables with more than or equal to
#'    this many unique values will not get discretized. Default is
#'    \code{granularity}.
#' @param ... a convenience for compatibility with its twin brother,
#'    restore_levels_fn.
#' @importFrom arules discretize
discretizer_fn <- function(column,
    granularity = 3, mode_freq_threshold = 0.15, mode_ratio_threshold = 1.5,
    category_range = min(granularity, 20):20, lower_count_bound = granularity,
    upper_count_bound = NULL, ...) {
  colname <- names(column)[[1]]
  column <- column[[1]]
  if (!is.numeric(column)) return(column)

  # Some caching optimizations
  uniques <- mungebitsTransformations:::present_uniques(column)
  if (!is.null(lower_count_bound) && length(uniques) <= lower_count_bound) return(column)
  if (!is.null(upper_count_bound) && length(uniques) >= upper_count_bound) return(column)
  variable_freqs <- mungebitsTransformations:::freqs(column, uniques)
  mode_value <- mungebitsTransformations:::Mode(column, uniques, variable_freqs)

  if (mean(column == mode_value, na.rm = TRUE) > mode_freq_threshold &&
      mungebitsTransformations:::mode_ratio(column, variable_freqs) > mode_ratio_threshold) {
    mode_corrected <- FALSE
    if (!is.null(category_range)) {
      for(i in category_range) {
        discretized_column <- try(suppressWarnings(arules:::discretize(column,
                                             method = 'frequency',
                                             categories = i)))
        if (inherits(discretized_column, 'try-error')) next 
        trimmed_levels <- gsub('^ *| *$', '', levels(discretized_column))
        if (mode_value %in% suppressWarnings(as.numeric(trimmed_levels))) {
          mode_corrected <- TRUE
          break
        }
      }
      if (!mode_corrected) {
        # TODO: Turn into binary variable

        warning(paste0("Mode of variable '", colname ,"' is above ", 100 * mode_freq_threshold, "% ",
                "and/or mode ratio is above ", mode_ratio_threshold, " and no number of buckets between ",
                min(category_range), " and ", max(category_range), " fixes the problem. May want to ",
                "discretize manually")) 
      }
    }
    if (!mode_corrected) {
      discretized_column <- try(arules:::discretize(column,
                                method = 'frequency',
                                categories = granularity))
      }
  } else {
    discretized_column <- try(arules:::discretize(column,
                                     method = 'frequency',
                                     categories = granularity))
  }

  # Handle weird discretizer bug
  # TODO: DO THIS IN RESTORE LEVELS
  if (is.list(discretized_column))
    discretized_column <- sapply(discretized_column, function(column) column[[1]])

  if (inherits(discretized_column, 'try-error'))
    stop(paste0("Problem discretizing variable '", colname, "': ", discretized_column))
  else {
    # Store the levels for restoring during prediction
    inputs$levels <<- levels(discretized_column)
    discretized_column
  }
}

restore_levels_fn <- function(column, ...) {
  if (!'levels' %in% names(inputs)) column[[1]]
  else {
    mungebitsTransformations:::numeric_to_factor(column[[1]], inputs$levels)
  }
}

#' Discretizer
#'
#' @param dataframe a data.frame to discretize.
#' @param input_cols a vector of columns to discretize.
#' @param ... the arguments passed to the discretization.
#' @export
discretizer <- column_transformation(function(column, verbose = FALSE, ...) {
  on.exit(inputs$trained <<- TRUE)
  fn <- if ('trained' %in% names(inputs)) mungebitsTransformations:::restore_levels_fn
        else mungebitsTransformations:::discretizer_fn
  environment(fn) <- environment() # Make inputs available
  if (verbose) {
    fn(column, ...)
  } else  {
    suppressMessages(suppressWarnings(fn(column, ...)))
  }
}, mutating = TRUE, named = TRUE)

# Some helper functions
mode_ratio <- function(variable,
                       variable_freqs = mungebitsTransformations:::freqs(variable)) {
  if (length(variable_freqs) < 2) stop('Cannot compute mode ratio of variable with ',
                              'less than 2 unique values.')
  variable_freqs[order(-variable_freqs)[1]] / variable_freqs[order(-variable_freqs)[2]]
}

# http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
Mode <- function(variable,
                 uniques = mungebitsTransformations:::present_uniques(variable),
                 variable_freqs = mungebitsTransformations:::freqs(variable, uniques)) {
  uniques[which.max(variable_freqs)]
}

present_uniques <- function(variable) {
  unique(variable[!is.na(variable)])
}

freqs <- function(variable,
                  uniques = mungebitsTransformations:::present_uniques(variable)) {
  tabulate(match(variable, uniques))
}

