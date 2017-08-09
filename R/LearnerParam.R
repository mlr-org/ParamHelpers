#' @title Create a description object for a parameter of a machine learning algorithm.
#'
#' @description
#' This specializes \code{\link{Param}} by adding a few more attributes,
#' like a default value, whether it refers to a training or a predict function, etc.
#' Note that you can set \code{length} to \code{NA}
#'
#' The S3 class is a \code{\link{Param}} which additionally stores these elements:
#' \describe{
#' \item{when [\code{character(1)}]}{See argument of same name.}
#' }
#'
#' See the note in \code{\link{Param}} about being able to pass expressions to certain arguments.
#'
#' @inheritParams Param
#' @param len [\code{integer(1)}]\cr
#'   Length of vector parameter.
#'   Can be set to \code{NA} to define a vector with unspecified length.
#' @param when [\code{character(1)}]\cr
#'   Specifies when parameter is used in the learner: \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \dQuote{train}.
#' @return [\code{\link{LearnerParam}}].
#' @name LearnerParam
#' @rdname LearnerParam
NULL

makeLearnerParam = function(p, when) {
  assertChoice(when, c("train", "predict", "both"))
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
