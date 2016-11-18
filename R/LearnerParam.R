#' Create a description object for a parameter of a machine learning algorithm.
#'
#' This specializes \code{\link{Param}} by adding a few more attributes,
#' like a default value, whether it refers to a training or a predict function, etc.
#'
#' The S3 class is a \code{\link{Param}} which additionally stores these elements:
#' \describe{
#' \item{default [any]}{See argument of same name.}
#' \item{has.default [\code{logical(1)}]}{Was a default value provided?}
#' \item{when [\code{character(1)}]}{See argument of same name.}
#' }
#'
#' @inheritParams Param
#' @param when [\code{character(1)}]\cr
#'   Specifies when parameter is used in the learner: \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \dQuote{train}.
#' @return [\code{\link{LearnerParam}}].
#' @name LearnerParam
#' @rdname LearnerParam
NULL

makeLearnerParam = function(p, when) {
  p$when = when
  class(p) = c("LearnerParam", "Param")
  return(p)
}

#' @export
print.LearnerParam = function(x, ..., trafo = TRUE, used = TRUE) {
  print.Param(x, trafo = trafo)
  if (used)
    catf("Used: %s.", x$when)
}
