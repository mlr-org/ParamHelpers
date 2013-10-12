\name{dfRowToList}
\alias{dfRowsToList}
\alias{dfRowToList}
\title{Convert a data.frame row to list of parameter-value-lists.}
\usage{
  dfRowToList(df, par.set, i)

  dfRowsToList(df, par.set)
}
\arguments{
  \item{df}{[\code{data.frame}]\cr Data.frame, potentially
  from \code{\link{OptPathDF}}.  Columns are assumed to be
  in the same order as par.set.}

  \item{par.set}{[\code{\link{ParamSet}}]\cr Parameter
  set.}

  \item{i}{[\code{integer(1)}]\cr Row index.}
}
\value{
  [\code{list}]. Named by parameter ids.
}
\description{
  Dependent parameters whose requirements are not satisfied
  are represented by a scalar NA in the output.
}

