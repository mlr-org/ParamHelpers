\name{generateGridDesign}
\alias{generateGridDesign}
\title{Generates a grid design for a parameter set.}
\usage{
generateGridDesign(n = 10L, par.set, trafo = FALSE)
}
\arguments{
  \item{n}{[\code{integer(k)}]\cr Resolution of the grid
  for each ot the \code{k} parameters in the
  \code{par.set}.}

  \item{par.set}{[\code{\link{ParamSet}}]\cr Parameter
  set.}

  \item{trafo}{[\code{logical(1)}]\cr Transform all
  parameters by using theirs respective transformation
  functions.  Default is \code{FALSE}.}
}
\value{
The created design is a data.frame. Columns are named by
the ids of the parameters.  If the \code{par.set} argument
contains a vector parameter, its corresponding column names
The result will have an \code{logical(1)} attribute
\dQuote{trafo}, which is set to the value of argument
\code{trafo}.
}
\description{
Generates a grid design for a parameter set.
}
\examples{
ps <- makeParamSet(
  makeNumericParam("x1", lower=-2, upper=1),
  makeIntegerParam("x2", lower=10, upper=20, trafo = function(x) x^(-2))
)
# simple gridDesign
generateGridDesign(c(4,5), ps, trafo = TRUE)
}

