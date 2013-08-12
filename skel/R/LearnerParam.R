#FIXME the dpocumentation has errors, reread it
#' Create a description object for a parameter of a machine learning algorithm.
#' 
#' This specializes \code{\link{Param}} by adding a few more attributes,
#' like a default value, whether it refers to a training or a predict function, etc.
#' 
#' The S3 class is a list which additionally stores these elements:
#' \describe{
#' \item{default [any]}{See argument of same name.}
#' \item{has.default [\code{logical(1)}]}{Was a default value provided? We cannot check \code{default} for \code{NULL} or \code{NA} as this could be the default value!}
#' \item{when [\code{numeric}]}{See argument of same name.}
#' }
#' 
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param len [\code{integer(1)}]\cr
#'   Length of vector.
#' @param lower [\code{numeric}]\cr
#'   Lower bound. 
#'   Default is \code{-Inf}.
#' @param upper [\code{numeric}]\cr
#'   Upper bound. 
#'   Default is \code{Inf}.
#' @param values [\code{vector} | \code{list}]\cr
#'   Possible discrete values. You are allowed to pass a list of complex R values,
#'   which are used as discrete choices. If you do the latter,
#'   the elements must be uniquely named, so that the names can be used
#'   as internal represenatation for the choice.    
#' @param default [any]\cr
#'   Default value used in learner. 
#'   If this argument is missing, it means no default value is available.
#' @param when [\code{character(1)}]\cr
#'   Specifies when parameter is used in the learner: \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#'   Default is \dQuote{train}.
#' @param requires [\code{expression}]\cr
#'   R expression over the other parameters to define requirements 
#'   when this parameter is effective. 
#' @return [\code{\link{LearnerParam}}].
#' @name LearnerParam
#' @rdname LearnerParam
NULL

makeLearnerParam = function(p, has.default, default, when, requires) {
  p$has.default = has.default
  p$default = default
  p$when = when
  p$requires = requires
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
