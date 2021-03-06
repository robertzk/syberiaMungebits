% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_rows.r
\name{select_rows}
\alias{select_rows}
\title{Select rows in a dataframe.}
\usage{
select_rows(dataframe, rows, whole = FALSE, ..., cols_to_check = NULL)
}
\arguments{
\item{dataframe}{a data.frame}

\item{rows}{an atomic vector or function. Drop all but these rows.
If \code{rows} is a function, the rows will be selected based on the
\code{whole} parameter. If \code{whole = TRUE}, the whole dataframe
will be passed in, and the resulting row indices (character, numeric,
or logical) will be selected. If \code{whole = FALSE} (the default)
the function will be applied to each row and the result will be expected
to be a logical, with only the rows returning \code{TRUE} being selected.}

\item{whole}{a logical. See the \code{rows} parameter. The default is 
\code{FALSE}.}

\item{...}{additional arguments to \code{rows} is that parameter is a function.}

\item{cols_to_check}{a character vector. Allows for rows to be applied to a
column subset of the dataframe. The default is to apply to all columns of
the dataframe.}
}
\description{
Select rows in a dataframe.
}
\examples{
select_rows(iris, 1:10) # Select only first ten rows
select_rows(iris, c(TRUE,FALSE)) # Select only odd rows
iris2 <- iris; rownames(iris2) <- paste0("row", 1:nrow(iris2))
select_rows(iris, c("row10", "row51")) # Select rows by name
}

