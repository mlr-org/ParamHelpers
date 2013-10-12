\name{paramValueToString}
\alias{paramValueToString}
\title{Convert a value to a string.}
\usage{
  paramValueToString(par, x, show.missing.values = FALSE)
}
\arguments{
  \item{par}{[\code{\link{Param}} |
  \code{\link{ParamSet}}]\cr Parameter or parameter set.}

  \item{x}{[any]\cr Value for parameter or value for
  parameter set. In the latter case it must be named list.}

  \item{show.missing.values}{[\code{logical(1)}]\cr Display
  \dQuote{NA} for parameters, which have no setting,
  because their requirements are not satified (dependent
  parameters), instead of nothing?  Default is
  \code{FALSE}.}
}
\value{
  [\code{character(1)}].
}
\description{
  Useful helper for logging. For discrete parameter values
  always the name of the discrete value is used.
}
\examples{
p <- makeNumericParam("x")
paramValueToString(p, 1) # "1.00"
paramValueToString(p, 1.2345) # "1.23"

p <- makeIntegerVectorParam("x", len=2)
paramValueToString(p, c(1L, 2L)) # 1,2

p <- makeLogicalParam("x")
paramValueToString(p, TRUE) # "TRUE"

p <- makeDiscreteParam("x", values=list(a=NULL, b=2))
paramValueToString(p, NULL) # "a"
}

