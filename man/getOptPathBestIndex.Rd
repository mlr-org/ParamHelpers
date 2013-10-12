\name{getOptPathBestIndex}
\alias{getOptPathBestIndex}
\title{Get index of the best element from optimization path.}
\usage{
  getOptPathBestIndex(op, y.name = op$y.names[1],
    dob = op$env$dob, eol = op$env$eol, ties = "last")
}
\arguments{
  \item{op}{[\code{\link{OptPath}}]\cr Optimization path.}

  \item{y.name}{[\code{character(1)}]\cr Name of target
  value to decide which element is best.  Default is
  \code{y.names[1]}.}

  \item{dob}{[\code{integer}]\cr Possible dates of birth to
  select best element from. Defaults to all.}

  \item{eol}{[\code{integer}]\cr Possible end of life to
  select best element from. Defaults to all.}

  \item{ties}{[\code{character(1)}]\cr How should ties be
  broken when more than one optimal element is found?
  \dQuote{all}: return all indices, \dQuote{first}: return
  first optimal element in path, \dQuote{last}: return last
  optimal element in path, \dQuote{random}: return random
  optimal element in path.  Default is \dQuote{last}.}
}
\value{
  [\code{integer}] Index or indices into path. See
  \code{ties}.
}
\description{
  Get index of the best element from optimization path.
}
\examples{
ps <- makeParamSet(makeNumericParam("x"))
op <- makeOptPathDF(par.set=ps, y.names="y", minimize=TRUE)
addOptPathEl(op, x=list(x=1), y=5)
addOptPathEl(op, x=list(x=2), y=3)
addOptPathEl(op, x=list(x=3), y=9)
addOptPathEl(op, x=list(x=4), y=3)
as.data.frame(op)
getOptPathBestIndex(op)
getOptPathBestIndex(op, ties="first")
}

