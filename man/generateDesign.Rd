\name{generateDesign}
\alias{generateDesign}
\title{Generates a statistical design for a parameter set.}
\usage{
  generateDesign(n = 10L, par.set, fun, fun.args = list(),
    trafo = FALSE, ints.as.num = FALSE,
    discretes.as.factor = TRUE, logicals.as.factor = FALSE)
}
\arguments{
  \item{n}{[\code{integer(1)}]\cr Number of samples in
  design.  Default is 10.}

  \item{par.set}{[\code{\link{ParamSet}}]\cr Parameter
  set.}

  \item{fun}{[\code{function}]\cr Function from package
  lhs.  Possible are: \code{\link[lhs]{maximinLHS}},
  \code{\link[lhs]{randomLHS}},
  \code{\link[lhs]{geneticLHS}},
  \code{\link[lhs]{improvedLHS}},
  \code{\link[lhs]{optAugmentLHS}},
  \code{\link[lhs]{optimumLHS}} Default is
  \code{\link[lhs]{randomLHS}}.}

  \item{fun.args}{[\code{list}]\cr List of further
  arguments passed to \code{fun}.}

  \item{trafo}{[\code{logical(1)}]\cr Transform all
  parameters by using theirs respective transformation
  functions.  Default is \code{FALSE}.}

  \item{ints.as.num}{[\code{logical(1)}]\cr Should
  parameters of type \dQuote{integer} or
  \dQuote{integervector} generate numeric columns?  Default
  is \code{FALSE}.}

  \item{discretes.as.factor}{[\code{logical(1)}]\cr Should
  discrete parameters have columns of type \dQuote{factor}
  in result?  Otherwise character columns are generated.
  Default is \code{TRUE}.}

  \item{logicals.as.factor}{[\code{logical(1)}]\cr Should
  logical parameters have columns of type \dQuote{factor}
  in result?  Otherwise logical columns are generated.
  Default is \code{FALSE}.}
}
\value{
  The created design is a data.frame. Columns are named by
  the ids of the parameters.  If the \code{par.set}
  argument contains a vector parameter, its corresponding
  column names in the design are the parameter id
  concatenated with 1 to dimension of the vector.  The data
  type of a column is defined in the following way. Numeric
  parameters generate numeric columns, integer parameters
  generate numeric/integer columns, logical parameters
  generate logical/factor columns.  For discrete parameters
  the value names are used and character or factor columns
  are generated.  Dependent parameters whose constaints are
  unsatisfied generate \code{NA} entries in their
  respective columns.  The result will have an
  \code{logical(1)} attribute \dQuote{trafo}, which is set
  to the value of argument \code{trafo}.
}
\description{
  Currently only lhs designs are supported.
}
\examples{
ps <- makeParamSet(
  makeNumericParam("x1", lower=-2, upper=1),
  makeIntegerParam("x2", lower=10, upper=20)
)
# random latin hypercube design with 5 samples:
generateDesign(5, ps)

# with trafo
ps <- makeParamSet(
  makeNumericParam("x", lower=-2, upper=1),
  makeNumericVectorParam("y", len=2, lower=0, upper=1, trafo=function(x) x/sum(x))
)
generateDesign(10, ps, trafo=TRUE)
}

