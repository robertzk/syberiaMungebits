#' numeric to factor helper function
#' 
#' TODO: Make this faster
#' 
#' @param num a numeric atomic vector. Will be restored to a factor variable.
#' @param levs a character vector of levels. 
#' @param na.to.missing a logical. Whether to convert NAs to a "Missing" level.
#' @keywords internal
numeric_to_factor <- function(num, levs, na.to.missing = TRUE) {
  if (length(levs) == 0) stop('Zero levels provided')
  if (length(num) > 1)
    return(sapply(num, function(n) { numeric_to_factor(n, levs, na.to.missing) }))

  if (na.to.missing && is.na(num))
    return(factor('Missing', levels = union(levs, 'Missing')))

  in_range_bools <- sapply(levs, function(lev) {
    lev <- as.character(lev)
    lev <- str_replace_all(lev, " ", "")
    lev_split <- strsplit(lev, ",")[[1]]
    if (length(lev_split) < 2) {
      old_opts <- options(warn = -1)
      on.exit(options(old_opts))
      level_to_num <- as.numeric(as.character(lev))
      return(!is.na(level_to_num) && level_to_num == num)
    }
    
    left_bound <- as.numeric(substr(lev_split[1], 2, nchar(lev_split[1])))
    right_bound <- as.numeric(substr(lev_split[2], 1, nchar(lev_split[2]) - 1))
    left_operator <- if (substr(lev, 1, 1) == '(') `<` else `<=`
    right_operator <- if (substr(lev, tmp <- nchar(lev), tmp) == ')') `>` else `>=`
    left_operator(left_bound, num) && right_operator(right_bound, num)
  })

  if (sum(in_range_bools) == 0) factor('Missing', levels = union(levs, 'Missing'))
  else factor(levs[in_range_bools], levels = levs)
}

