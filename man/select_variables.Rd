\name{select_variables}
\alias{select_variables}
\title{Select variables in a dataframe.}
\usage{
select_variables(dataframe, cols, weak = TRUE)
}
\arguments{
  \item{dataframe}{a data.frame}

  \item{cols}{an atomic vector. Drop all but these
  columns.}

  \item{weak}{logical. Whether or not to add non-existent
  columns as NAs.}
}
\description{
Select variables in a dataframe.
}
\examples{
select_variables(iris, 1) # Select only first variable
select_variables(iris, c('Sepal.Length', 'Petal.Length'))
select_variables(iris, c(T,T,F,F,T)) # Exclude cols 3 and 4
}

