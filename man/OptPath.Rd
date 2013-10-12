\name{makeOptPathDF}
\alias{makeOptPathDF}
\alias{OptPath}
\alias{OptPathDF}
\title{Create optimization path.}
\usage{
  makeOptPathDF(par.set, y.names, minimize,
    add.transformed.x = FALSE)
}
\arguments{
  \item{par.set}{[\code{\link{ParamSet}}]\cr Parameter set
  for the decision variables that are optimized.}

  \item{y.names}{[\code{character}]\cr Names of performance
  measures that are optimized or logged.}

  \item{minimize}{[\code{logical}]\cr Which of the
  performance measures in y.names should be minimized?
  Vector of booleans in the same order as \code{y.names}.}

  \item{add.transformed.x}{[\code{logical(1)}]\cr If some
  parameters have associated transformations, are you going
  to add x values after they have been transformed?
  Default is \code{FALSE}.}
}
\description{
  Optimizers can iteratively log their evaluated points
  into this object. Can be converted into a data.frame with
  \code{as.data.frame(x, discrete.as.factor = TRUE /
  FALSE)}.
}
\details{
  A optimization path has a number of path elements, where
  each element consists of: the value of the decision
  variables at this point, the values of the performance
  measures at this point, the date-of-birth (dob) of this
  point and the end-of-life (eol) of this point.

  For discrete parameters always the name of the value is
  stored as a character. When you retrieve an element with
  \code{\link{getOptPathEl}}, this name is converted to the
  actual discrete value.

  If parameters have associated transformation you are free
  to decide whether you want to add x values before or
  after transformation, see argument
  \code{add.transformed.x} and \code{\link{trafoOptPath}}.

  The S3 class is a list which stores at least these
  elements: \describe{ \item{par.set
  [\code{\link{ParamSet}}]}{See argument of same name.}
  \item{y.names [\code{character}]}{See argument of same
  name.} \item{minimize [\code{logical}]}{See argument of
  same name.} \item{add.transformed.x
  [\code{logical(1)}]}{See argument of same name.}
  \item{env [\code{environment}]}{Environment which stores
  the optimization path. Contents depend on
  implementation.} }
}
\seealso{
  \code{\link{getOptPathLength}},
  \code{\link{getOptPathEl}}, \code{\link{addOptPathEl}},
  \code{\link{getOptPathY}}, \code{\link{setOptPathElDOB}},
  \code{\link{setOptPathElEOL}}
}

