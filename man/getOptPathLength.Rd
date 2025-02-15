% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OptPath_getter.R
\name{getOptPathLength}
\alias{getOptPathLength}
\title{Get the length of the optimization path.}
\usage{
getOptPathLength(op)
}
\arguments{
\item{op}{\link{OptPath}\cr
Optimization path.}
}
\value{
\code{integer(1)}
}
\description{
Dependent parameters whose requirements are not satisfied are represented by a scalar
NA in the output.
}
\seealso{
Other optpath: 
\code{\link{OptPath}},
\code{\link{addOptPathEl}()},
\code{\link{getOptPathBestIndex}()},
\code{\link{getOptPathCol}()},
\code{\link{getOptPathCols}()},
\code{\link{getOptPathDOB}()},
\code{\link{getOptPathEOL}()},
\code{\link{getOptPathEl}()},
\code{\link{getOptPathErrorMessages}()},
\code{\link{getOptPathExecTimes}()},
\code{\link{getOptPathParetoFront}()},
\code{\link{getOptPathX}()},
\code{\link{getOptPathY}()},
\code{\link{setOptPathElDOB}()},
\code{\link{setOptPathElEOL}()}
}
\concept{optpath}
