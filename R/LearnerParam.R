#' @title Create a description object for a parameter of a machine learning
#'   algorithm.
#'
#' @description This specializes [Param()] by adding a few more attributes, like
#' a default value, whether it refers to a training or a predict function, etc.
#' Note that you can set `length` to `NA`
#'
#' The S3 class is a [Param()] which additionally stores these elements:
#' \describe{
#' \item{when `character(1)`}{See argument of same name.}
#' }
#'
#' See the note in [Param()] about being able to pass expressions to certain arguments.
#'
#' @inheritParams Param
#' @param len (`integer(1)`)\cr
#'   Length of vector parameter.
#'   Can be set to `NA` to define a vector with unspecified length.
#' @param when (`character(1)`)\cr
#'   Specifies when parameter is used in the learner: \dQuote{train},
#'   \dQuote{predict} or \dQuote{both}. Default is \dQuote{train}.
#' @return [LearnerParam()].
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
  if (used) {
    catf("Used: %s.", x$when)
  }
}
