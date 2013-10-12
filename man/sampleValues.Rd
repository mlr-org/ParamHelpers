\name{sampleValues}
\alias{sampleValues}
\title{Sample n random values from a parameter or a parameter set uniformly.}
\usage{
  sampleValues(par, n, discrete.names = FALSE)
}
\arguments{
  \item{par}{[\code{\link{Param}} |
  \code{\link{ParamSet}}]\cr Parameter or parameter set.}

  \item{n}{[\code{integer(1)}]\cr Number of values.}

  \item{discrete.names}{[\code{logical(1)}]\cr Should names
  be sampled for discrete / logical paramaters or values
  instead?  Default is code {FALSE}.}
}
\value{
  [\code{list}]. For consistency always a list is returned.
}
\description{
  Dependent parameters whose requirements are not satisfied
  are represented by a scalar NA in the output.
}
\examples{
p <- makeIntegerParam("x", lower=-10, upper=10)
sampleValues(p, 4)

p <- makeNumericParam("x", lower=-10, upper=10)
sampleValues(p, 4)

p <- makeLogicalParam("x")
sampleValues(p, 4)

ps <- makeParamSet(
  makeNumericParam("u", lower=1, upper=10),
  makeIntegerParam("v", lower=1, upper=10),
  makeDiscreteParam("w", values=1:2)
)
sampleValues(ps, 2)
}

