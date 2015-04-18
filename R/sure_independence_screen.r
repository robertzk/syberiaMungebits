#' Sure independence screening function
#'
#' @param dataframe a data.frame. Run sure independence screening on this
#'   dataframe.
#' @param ... further SIS options. See also \code{sure_independence_screening} in
#'   the \code{statsUtils} package.
#' @param exclude character. A vector of variables to never remove.
#' @param discretizer_params list. A list of parameters to pass to the \code{discretizer}
#'   that discretizes all non-categorical variables before sure independence screening.
#'   By default, these are \code{granularity = N}, \code{upper_count_bound = NULL},
#'   \code{lower_count_bound = 1}, where \code{N} is the rounded mean of the number
#'   of levels in each existent factor variable (i.e. if some were pre-discretized),
#'   or 3 if there are less than 5 such variables. If any are left undiscretized,
#'   these will not be screened.
#' @importFrom statsUtils sure_independence_screening
#' @export
sure_independence_screen <- function(dataframe, ..., exclude = character(0),
                                     max_levels = 100,
                                     discretizer_params = list()) {
  if (!'remaining_columns' %in% names(inputs)) {
    # To use statsUtils::sure_independence_screening, we must discretize all variables
    discretize_cols <- names(which(vapply(dataframe, Negate(is.factor), logical(1))))
    discretize_cols <- setdiff(discretize_cols, exclude)

    sisdf <- dataframe[, setdiff(colnames(dataframe), exclude)]
    categorical_variables <- vapply(dataframe, is.factor, logical(1))

    # Calculate the rounded mean of the number  of levels in each existent factor
    # variable (i.e. if some were pre-discretized), or 3 if there are less than 5
    # such variables.
    granularity <- if (sum(categorical_variables) < 5) 3
      else round(mean(vapply(dataframe[, categorical_variables], nlevels, integer(1))))

    discretizer_params <- mungebits:::list_merge(discretizer_params, list(
      granularity = granularity, upper_count_bound = NULL, lower_count_bound = 1))

    mb <- mungebits:::mungebit(discretizer)
    sisdf <- mungebits:::mungeplane(sisdf)
    # Using do.call doesn't work here because discretizer is a column transformation
    # and so uses non-standard evaluation.
    mb$run(sisdf, discretize_cols, granularity = discretizer_params$granularity,
      upper_count_bound = discretizer_params$upper_count_bound,
      lower_count_bound = discretizer_params$lower_count_bound)

    # Raise an exception if the number of levels exceeds 100
    if (any(violations <- vapply(df <- dataframe[,!names(dataframe) %in% exclude], function(x) is.factor(x) &&
          nlevels(x) > max_levels, logical(1)))) {
      stop("Too many levels in ", paste(colnames(df)[violations], collapse = ', '))
    }

    # Replace NAs with a new level, "Missing", so that sure independence screening
    # can pick up on potential significance / insignificance of missing values.
    (mungebits:::mungebit(value_replacer))$run(sisdf, is.factor, list(list(NA, 'Missing')))

    mungebits:::mungebit(column_transformation(eval(bquote(function(column, ...) {
      if (is.null(statsUtils::sure_independence_screening(.(dataframe$dep_var), column, ...))) NULL
      else column
    }))))$run(sisdf, setdiff(colnames(sisdf$data), exclude), ...)

    inputs$remaining_columns <<- c(exclude, colnames(sisdf$data))
  }
  inputs$remaining_columns <- intersect(inputs$remaining_columns, colnames(dataframe))
  removed <- setdiff(colnames(dataframe), inputs$remaining_columns)
  eval.parent(substitute(for (varname in removed) dataframe[[varname]] <- NULL))
  TRUE
}

