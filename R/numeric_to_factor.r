#' numeric to factor helper function
#' 
#' TODO: Make this faster
#' 
#' @param num a numeric atomic vector. Will be restored to a factor variable.
#' @param levs a character vector of levels. 
#' @param na.to.missing a logical. Whether to convert NAs to a "Missing" level.
#' @export
#' @keywords internal
numeric_to_factor <- function(num, levs, na.to.missing = TRUE) {
  .Call('mungebitsTransformations_numeric_to_factor',
         num, levs, na.to.missing, PACKAGE = 'mungebitsTransformations')
}

numeric_to_factor2 <- function(num, levs, na.to.missing = TRUE) {
  if (length(levs) == 0) stop('Zero levels provided')

  levs <- as.character(levs)
  original_levs <- levs

  levs <- str_replace_all(levs, ' ', '')
  levs <- strsplit(levs, ',')
  numeric_levs <- vapply(levs,
    function(lev) length(lev) == 1 &&
      all(strsplit(lev, '')[[1]]  %in% strsplit('-.0123456789', '')[[1]]),
    logical(1))

  charnums <- character(length(num))
  already_numeric <- as.character(num) %in% levs[numeric_levs]
  charnums[already_numeric] <- num[already_numeric]

  range_levs <- vapply(levs, function(lev) length(lev) == 2, logical(1))
  levs[range_levs] <- lapply(levs[range_levs],
    function(lev) {
      left_bound <- as.numeric(substr(lev[1], 2, nchar(lev[1])))
      right_bound <- as.numeric(substr(lev[2], 1, nchar(lev[2]) - 1))
      left_operator <- if (substr(lev[1], 1, 1) == '(') `<` else `<=`
      right_operator <- if (substr(lev[2], tmp <- nchar(lev[2]), tmp) == ')') `>` else `>=`
      list(function(x) left_operator(left_bound, x),
           function(x) right_operator(right_bound, x))
    })

  in_levs <- lapply(levs[range_levs], function(lev) {
    lev[[1]](num[!already_numeric]) & lev[[2]](num[!already_numeric])
  })
  in_levs <- apply(data.frame(in_levs), 1, function(ins) which(c(ins, TRUE))[1])

  charnums[!already_numeric] <- c(original_levs[range_levs], NA)[in_levs]

  final_levs <- original_levs
  if (na.to.missing && sum(is.na(charnums)) > 0)
    final_levs <- union(original_levs, 'Missing')

  if (na.to.missing) charnums[is.na(charnums)] <- 'Missing'

  factor(charnums, levels = final_levs)
}

