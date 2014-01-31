\name{select_rows}
\alias{select_rows}
\title{Select rows in a dataframe.}
\usage{
select_rows(dataframe, rows, whole = FALSE)
}
\arguments{
  \item{dataframe}{a data.frame}

  \item{rows}{an atomic vector. Drop all but these rows.}
}
\description{
Select rows in a dataframe.
}
\examples{
select_rows(iris, 1:10) # Select only first ten rows
select_rows(iris, c(T,F)) # Select only odd rows
iris2 <- iris; rownames(iris2) <- paste0("row", 1:nrow(iris2))
select_rows(iris, c("row10", "row51") # Select rows by name
}

