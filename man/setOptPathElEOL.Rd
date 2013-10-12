\name{setOptPathElEOL}
\alias{setOptPathElEOL}
\title{Set the end of life dates of parameter values, in-place.}
\usage{
  setOptPathElEOL(op, index, eol)
}
\arguments{
  \item{op}{[\code{\link{OptPath}}]\cr Optimization path.}

  \item{index}{[\code{integer}]\cr Vector of indices of
  elements.}

  \item{eol}{[integer] \cr EOL dates, single value or same
  length of \code{index}.}
}
\value{
  Nothing.
}
\description{
  Set the end of life dates of parameter values, in-place.
}

