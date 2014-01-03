\name{date_parser_fn}
\alias{date_parser_fn}
\title{Parse string dates into POSIXct}
\usage{
date_parser_fn(col)
}
\arguments{
  \item{col}{an atomic vector. The column to process.}
}
\value{
numeric. Number of years
}
\description{
This is a column transformation and will be wrapped in
\code{column_transformation}.
}
\examples{
dd <- data.frame(cbind(c("", "8 days", "20 days")), stringsAsFactors = FALSE)
date_parser(dd, 1)
print(dd[[1]])
# [1] 0 8 20
}

