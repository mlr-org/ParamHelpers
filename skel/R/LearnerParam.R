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
#' @param id [\code{character(1)}]\cr
#'   See \code{\link{Param}}.
#' @param len [\code{integer(1)}]\cr
#'   See \code{\link{Param}}.
#' @param lower [\code{numeric}]\cr
#'   See \code{\link{Param}}.
#' @param upper [\code{numeric}]\cr
#'   See \code{\link{Param}}.
#' @param values [\code{vector} | \code{list}]\cr
#'   See \code{\link{Param}}.
#' @param requires [\code{NULL} | R expression]\cr
#'   See \code{\link{Param}}.
#' @param default [any]\cr
#'   Default value used in learner. 
#'   If this argument is missing, it means no default value is available.
#' @param when [\code{character(1)}]\cr
#'   Specifies when parameter is used in the learner: \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \dQuote{train}.
#' @return [\code{\link{LearnerParam}}].
#' @name LearnerParam
#' @rdname LearnerParam
NULL

makeLearnerParam = function(p, has.default, default, when) {
  #We cannot check default} for NULL or NA as this could be the default value!
  p$has.default = has.default
  #FIXME: Do we need to check for NA here? hopefully not because this might occur in mlr?
  if (isScalarNA(default)) 
    warningf("NA used as a default value for learner parameter %s.\nParamHelpers uses NA as a special value for dependent parameters.", p$id)
  p$default = default
  p$when = when
  class(p) = c("LearnerParam", "Param")
  return(p)
}

#' @S3method print LearnerParam
print.LearnerParam = function(x, ...) {
  print.Param(x)
  def = if(is.null(x$default)) 
    "<none>" 
  else 
    paramValueToString(x, x$default)
  catf("Used: %s. Default: %s.", x$when, def)
}
