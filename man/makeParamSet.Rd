\name{makeParamSet}
\alias{makeNumericParamSet}
\alias{makeParamSet}
\alias{ParamSet}
\title{Construct a parameter set.}
\usage{
  makeParamSet(..., params)

  makeNumericParamSet(id = "x", len, lower = -Inf,
    upper = Inf, vector = TRUE)
}
\arguments{
  \item{...}{[\code{\link{Param}}]\cr Parameters.}

  \item{params}{[list of \code{\link{Param}}]\cr List of
  parameters, alternative way instead of using \code{...}.}

  \item{id}{[\code{character(1)}] Name of parameter.}

  \item{len}{[\code{integer(1)}]\cr Length of vector.}

  \item{lower}{[\code{numeric}]\cr Lower bound.  Default is
  \code{-Inf}.}

  \item{upper}{[\code{numeric}] \cr Upper bound.  Default
  is \code{Inf}.}

  \item{vector}{[\code{logical(1)}] \cr Should a
  \code{NumericVectorParam} be used instead of n
  \code{NumericParam} objects?  Default is \code{TRUE}.}
}
\value{
  [\code{\link{ParamSet}}].
}
\description{
  \code{makeParamSet}: Contruct from a bunch of parameters.

  \code{makeNumericParamSet}: Convenience function for
  numerics.
}
\details{
  Multiple sets can be concatenated with \code{c}.

  The constructed S3 class is simply a list that contains
  the element \code{pars}. \code{pars} is a list of the
  passed parameters, named by their ids.
}
\examples{
makeParamSet(
  makeNumericParam("u", lower=1),
  makeIntegerParam("v", lower=1, upper=2),
  makeDiscreteParam("w", values=1:2),
  makeLogicalParam("x"),
  makeDiscreteVectorParam("y", len=2, values=c("a", "b"))
)
}

