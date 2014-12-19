if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

standard_dataframe_attributes <- c('names', 'row.names', 'class')

