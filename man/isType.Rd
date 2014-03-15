\name{isType}
\alias{isDiscrete}
\alias{isInteger}
\alias{isNumeric}
\alias{isType}
\title{Check parameter / parameter set contain ONLY a certain type.}
\usage{
isNumeric(par, include.int = TRUE)

isDiscrete(par)

isInteger(par)
}
\arguments{
  \item{par}{[\code{\link{Param}} |
  \code{\link{ParamSet}}]\cr Parameter or parameter set.}

  \item{include.int}{[\code{logical(1)}]\cr Are integers
  also considered to be numeric?  Default is \code{TRUE}.}
}
\value{
[\code{logical(1)}].
}
\description{
Check parameter / parameter set contain ONLY a certain
type.
}

