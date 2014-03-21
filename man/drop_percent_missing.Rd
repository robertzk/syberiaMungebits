\name{drop_percent_missing}
\alias{drop_percent_missing}
\title{Drop columns with over a certain percent missing.}
\usage{
drop_percent_missing(dataframe, input_cols = colnames(dataframe), ...)
}
\arguments{
  \item{x}{any. The column to screen.}

  \item{threshold}{numeric. The percent (e.g., 0.5) below
  which to remove column that have at least that percent
  missing. The default is 0.8.}
}
\description{
Drop columns with over a certain percent missing.
}
\examples{
df <- iris; df[, 6] <- NA; df[1, 6] <- 1
drop_percent_missing(df, , 0.8) # Drop sixth column
}

