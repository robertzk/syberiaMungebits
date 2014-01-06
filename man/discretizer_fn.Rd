\name{discretizer_fn}
\alias{discretizer_fn}
\title{Discretizer function}
\usage{
discretizer_fn(column, granularity = 3, mode_freq_threshold = 0.15,
  mode_ratio_threshold = 1.5, category_range = min(granularity, 20):20,
  lower_count_bound = granularity, ...)
}
\arguments{
  \item{column}{an atomic vector. The variable to
  discretize.}

  \item{granularity}{an integer. The suggested number of
  levels.}

  \item{mode_freq_threshold}{a real value between 0 and 1.
  If the mode of the variable exceeds this value and is
  greater than \code{mode_ratio_threshold} (see next
  parameter) times the next greatest mode (i.e., the ratio
  of the value occuring most often over the value occuring
  second most often is over \code{mode_ratio_threshold})
  then the variable will be attempted to be discretized in
  manner as to make the mode its own bucket. (so if the
  mode is 5, we'd want, e.g., [2,4), 5, and (5, 7]).}

  \item{mode_ratio_threshold}{a real value. See the
  \code{mode_freq_threshold} parameter.}

  \item{category_range}{The number of levels to consider
  when the discretization procedure descrized in the
  \code{mode_freq_threshold} parameter is employed. The
  default is \code{min(granularity, 20):20}.}

  \item{lower_count_bound}{an integer. Variables with less
  than or equal to this many unique values will not get
  discretized. Default is \code{granularity}.}

  \item{...}{a convenience for compatibility with its twin
  brother, restore_levels_fn.}
}
\description{
Discretizer function
}

