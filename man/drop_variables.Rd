\name{drop_variables}
\alias{drop_variables}
\title{Drop variables in a dataframe.}
\usage{
drop_variables(x)
}
\arguments{
  \item{x}{an atomic vector. The column to process.}
}
\value{
NULL ensures the column gets removed
}
\description{
This is a column transformation and will be wrapped in
\code{column_transformation}.
}
\examples{
drop_variables(iris, 1) # Drop first column
drop_variables(iris, c('Sepal.Length', 'Petal.Length'))
drop_variables(iris, c(T,T,F,F,T)) # only leave columns 3 and 4
}

