#' @title Check if parameter values contain expressions.
#'
#' @description Checks if a parameter, parameter set or list of parameters
#'   contain expressions.
#' @param obj ([Param()] | `ParamHelpers::ParamSet()` | `list`)\cr
#'   Parameter, parameter set or list of parameters.
#' @return `logical(1)`.
#' @examples
#' ps1 = makeParamSet(
#'   makeNumericParam("x", lower = 1, upper = 2),
#'   makeNumericParam("y", lower = 1, upper = 10)
#' )
#'
#' ps2 = makeParamSet(
#'   makeNumericLearnerParam("x", lower = 1, upper = 2),
#'   makeNumericLearnerParam("y", lower = 1, upper = expression(p))
#' )
#'
#' hasExpression(ps1)
#' hasExpression(ps2)
#' @export
hasExpression = function(obj) {
  UseMethod("hasExpression")
}

#' @export
hasExpression.Param = function(obj) {
  any(vlapply(obj, is.expression))
}

#' @export
hasExpression.LearnerParam = function(obj) {
  any(vlapply(obj, is.expression))
}

#' @export
hasExpression.ParamSet = function(obj) {
  any(vlapply(obj$pars, hasExpression))
}

#' @export
hasExpression.LearnerParamSet = function(obj) {
  any(vlapply(obj$pars, hasExpression))
}

#' @export
hasExpression.list = function(obj) {
  any(vlapply(obj, hasExpression))
}
