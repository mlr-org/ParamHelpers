\name{discretizeParam}
\alias{discretizeParam}
\title{Discretesizes a given parameter to a sequence of values.}
\usage{
discretizeParam(par, lower, upper, by, length.out, trafo = FALSE)
}
\arguments{
  \item{par}{[\code{\link{Param}}] The Parameter you want
  to be discretized.}

  \item{lower}{[\code{numeric}(1)] Lower bound. Will
  overwrite the value in Param if set.}

  \item{upper}{[\code{numeric}(1)] Upper bound. Will
  overwrite the value in Param if set.}

  \item{by}{[\code{numeric}(1)] Increment of the sequence.}

  \item{length.out}{[\code{numeric}(1)] Desired length of
  the sequence.}

  \item{trafo}{[\code{boolean}(1)] Should the
  transformation be applied?}
}
\value{
[\code{\link{Param}}] A \code{Param} of the type
\code{discrete}.
}
\description{
Discretesizes a given parameter to a sequence of values.
}
\examples{
par <- makeNumericParam("x1", lower=-2, upper=1)
disc.par <- discretizeParam(par, by=0.25)
}

