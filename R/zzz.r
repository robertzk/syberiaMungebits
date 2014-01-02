.onLoad <- function(...) {
  date_parser <<- column_transformation(.date_parser)
  drop_single_value_variables <<- column_transformation(.drop_single_value_variables)
  drop_variables <<- column_transformation(.drop_variables)
  value_replacer <<- column_transformation(.value_replacer)
  imputer <<- column_transformation(.imputer)
}
