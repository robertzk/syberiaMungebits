#' Old-to-new column mapping transformation
#'
#' A mungebit which does not affect any existing columns, but can transform
#' other columns into new columns, or affect multiple columns simultaneously
#' as functions of each other.
#'
#' @param transformation a function. The arguments will be the ordered columns
#'    selected from the dataframe when the transformation is performed (see
#'    examples). One has to be careful to return an atomic vector if the
#'    result is 1-dimensional, and a list otherwise.
#' @return a function which takes a data.frame, input columns, and output
#'    columns, the latter two of which specify the domain and range of the
#'    transformation (see examples).
#' @seealso \code{\link{column_transformation}}
#' @export
#' @examples
#' perimeter <- multi_column_transformation(function(x, y) 2*x + 2*y)
#' perimeter(iris, c('Sepal.Length', 'Sepal.Width'), 'Sepal.Perimeter')
#'
#' multiplier <- multi_column_transformation(function(x, y) x*y)
#' multiplier(iris, c('Sepal.Length', 'Sepal.Width'), 'Sepal.Area')
#'
#' property_generator <- multi_column_transformation(
#'    function(x, y) list(2*x + 2*y, x*y))
#' property_generator(iris, c('Sepal.Length', 'Sepal.Width'),
#'    c('Sepal.Perimeter', 'Sepal.Area'))
#'
#' swapper <- multi_column_transformation(function(x, y) list(y, x))
#' swapper(iris, c('Sepal.Length', 'Sepal.Width'),
#'   c('Sepal.Width', 'Sepal.Length'))
#'
#' column_remover <- multi_column_transformation(function(...) NULL)
#' column_remover(iris, c('Sepal.Length', 'Sepal.Width'))
#'
#' scaling_fun <- function(...) {
#'   args <- list(...)
#'   const <- args[[length(args)]]
#'   args <- head(args, -1)
#'   if (length(args) == 1) args[[1]] * const
#'   else lapply(args, function(col) col * const)
#' }
#' scaler <- multi_column_transformation(scaling_fun)
#' # scale Sepal.Length and Sepal.Width by two
#' scaler(iris, c('Sepal.Length', 'Sepal.Width'), , 2)
#' scaler(iris[c('Petal.Length', 'Petal.Width')], , , 2)
#' # Note the missing second and third arguments.
multi_column_transformation <- function(transformation) {
  invisible(structure(function(dataframe, input_cols = colnames(dataframe),
           output_cols = input_cols, suffixes = c(), ...) {
    
    # If input_cols is any of the following functions:
    #   is.numeric, is.double, is.factor, is.character
    # then assume the transformation is one-to-many and apply it over each column
    if (identical(input_cols,is.numeric) ||
        identical(input_cols,is.double) ||
        identical(input_cols,is.factor) ||
        identical(input_cols,is.character)) { # input_cols is a function
      
        # this is basically a synonym for the current function
        my_mct <- multi_column_transformation(transformation)
      
        dataframe <- substitute(dataframe)
        invisible(eval(substitute({
            
          # get list of variables in dataframe
          whichcols <- names(dataframe)[which(sapply(dataframe,input_cols))]

          # do the transformation for each column
          for (col in whichcols) {
            newcol <- paste0(col,"TMP")
            indcol <- paste0(col,suffixes)
            my_mct(dataframe, col, c(newcol,indcol,col))
          }
          
          # Note about suffix parameter:
          # It is expected that the first column returned by the one-to-many transformation
          # will have the same name as the input column, and all others will be denoted by
          # some suffix.  Thus if we expect 4 output columns, we would specify three suffixes:
          #  suffix = c(_1,_2,_3) 
          # The additional columns would then be labeled as follows:
          #  x -> (x, x_1, x_2, x_3)
          
          # now restore the names of the imputed columns
          origcols <- whichcols
          newcols <- paste0(whichcols,'TMP')
          replacements <- origcols
          names(replacements) <- newcols
          replacements <- as.list(replacements)
          renamer(dataframe,replacements)
                 
          NULL
          
        }), envir = parent.frame()))
    } else { # input_cols is a list of column names

      # The fastest way to do this. The alternative is to use pass by value
      # or replace list subset assignment below with mapply.

      # Only assign *tmp.fn.left.by.mungebits.library* if the transformation is a normal function
      transformation_is_itself_a_multicolumntranformation <- "multiColumnTransformation" %in% class(transformation)
      if (transformation_is_itself_a_multicolumntranformation) {
        #assign("*tmp.fn.left.by.mungebits.library*", transformation, envir = parent.frame())
      } else {
        assign("*tmp.fn.left.by.mungebits.library*", transformation, envir = parent.frame())
      }
      print(environment(transformation))
      print(environment(`*tmp.fn.left.by.mungebits.library*`))
      
      print( transformation_is_itself_a_multicolumntranformation)
      #print(transformation)
      #print(class(transformation))
      
      input_cols <- force(input_cols)
      if (is.logical(input_cols)) input_cols <- which(input_cols)
      output_cols <- force(output_cols)
      if (is.logical(output_cols)) output_cols <- which(output_cols)
  
      dataframe <- substitute(dataframe)
      invisible(eval(substitute({
        # Trick to make assignment incredibly fast. Could screw up the
        # data.frame if the function is interrupted, however.
        class(dataframe) <- 'list'
        on.exit(class(dataframe) <- 'data.frame')
  
        # Unfortunately, due to the wrapped substitute causing scoping issues,
        # the clever
        #   (if (length(output_cols) == 1) `[[<-` else `[<-`)(dataframe, ...)
        # does not work here.
        if (length(output_cols) == 1)
          dataframe[[output_cols]] <-
            do.call(`*tmp.fn.left.by.mungebits.library*`,
                    append(unname(dataframe[input_cols]), list(...)))
        else 
          dataframe[output_cols] <-
            do.call(`*tmp.fn.left.by.mungebits.library*`,
                    append(unname(dataframe[input_cols]), list(...)))
  
        #for(col in colnames(iris2)[vapply(iris2[1,], is.null, logical(1))]) iris2[[col]] <- NULL 
        # Keeping some alternative code for now:
        # Remove any columns that were set to NULL explicitly
        #which(vapply(output_cols, function(x) is.null(dataframe[[x]]), logical(1))) 
        # dataframe[which(vapply(output_cols, function(x) is.null(dataframe[[x]]), logical(1))) %||% integer(0)] <- NULL
        dataframe[seq_along(dataframe)[
          vapply(seq_along(dataframe),
                 function(x) is.null(dataframe[[x]]),
                 logical(1))
        ]] <- NULL
        #dataframe[output_cols[
        #  vapply(intersect(output_cols, names(dataframe)),
        #         function(x) is.null(dataframe[[x]]),
        #         logical(1))
        #]] <- NULL
  
        class(dataframe) <- 'data.frame'
        NULL
      }), envir = parent.frame()))
    }
  }, class = c('multiColumnTransformation', 'transformation', 'function')
  # TODO: named = named, mutating = mutating)
  ))
}

##' @export
# MCT <- multi_column_transformation



# TESTING PURPOSES
my_mct <- multi_column_transformation(missing_indicator)
x <- 1:10; x[4] <- NA
y <- 10:1; y[8] <- NA
dataframe <- data.frame(x,y)
my_mct(dataframe, is.numeric, suffix="_missing"); print(dataframe)

# make some fake data
x <- 1:10
y <- c('a','b','c','d','e','f','g','h','i','j')
z <- 3:12
dataframe <- data.frame(x,y,z)

my_mct <- multi_column_transformation(function(x) list(x,x+1,2*x,NULL))
#my_mct(dataframe, is.numeric, suffixes=c("_1","_2")); print(dataframe)

