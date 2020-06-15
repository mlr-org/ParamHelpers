#' @title Evaluates all expressions within a parameter.
#'
#' @description Evaluates the expressions of a parameter, parameter set or list
#' of parameters for a given dictionary.
#'
#' @param obj ([Param()] | `ParamHelpers::ParamSet()` | `list`)\cr
#'   Parameter, parameter set or list of parameter values. Expressions within
#'   `len`, `lower` or `upper` boundaries, `default` or `values` will be
#'   evaluated using the provided dictionary (`dict`).
#' @template arg_dict
#' @return [[Param()] | `ParamHelpers::ParamSet()` | `list`].
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("x", lower = expression(p), upper = expression(ceiling(3 * p))),
#'   makeIntegerParam("y", lower = 1, upper = 2)
#' )
#' evaluateParamExpressions(ps, dict = list(p = 3))
#'
#' ps = makeParamSet(
#'   makeNumericParam("x", default = expression(sum(data$Species == "setosa"))),
#'   makeIntegerParam("y", lower = 1, upper = 2),
#'   keys = c("data", "Species")
#' )
#' evaluateParamExpressions(ps, dict = list(data = iris))
#'
#' par.vals = list(
#'   x = expression(k),
#'   y = 5
#' )
#' evaluateParamExpressions(par.vals, dict = list(k = 3))
evaluateParamExpressions = function(obj, dict = NULL) {
  UseMethod("evaluateParamExpressions")
}

#' @export
evaluateParamExpressions.Param = function(obj, dict = NULL) {

  assertClass(obj, "Param")
  if (!hasExpression(obj)) {
    return(obj)
  }
  assertList(dict, names = "unique", null.ok = TRUE)
  # replace expressions in length (needs to be done prior to computing
  # defaults, values and boundaries)
  length = getParamLengths(par = obj, dict = dict)
  obj$len = asInt(length, na.ok = TRUE)

  # replace expressions in default, values and boundaries
  if (!is.null(obj$lower)) {
    obj$lower = unname(getLower(obj = obj, dict = dict))
  }
  if (!is.null(obj$upper)) {
    obj$upper = unname(getUpper(obj = obj, dict = dict))
  }
  if (!is.null(obj$default)) {
    obj$default = getDefaults(obj = obj, dict = dict)
  }
  if (!is.null(obj$values)) {
    obj$values = getValues(obj = obj, dict = dict)
  }
  return(obj)
}

#' @export
evaluateParamExpressions.ParamSet = function(obj, dict = NULL) {
  assertClass(obj, "ParamSet")
  if (!hasExpression(obj)) {
    return(obj)
  }
  assertList(dict, names = "unique", null.ok = TRUE)
  ids = names(obj$pars)
  # evaluate all parameters separately
  obj$pars = lapply(obj$pars, function(par) {
    evaluateParamExpressions(obj = par, dict = dict)
  })
  names(obj$pars) = ids
  return(obj)
}

#' @export
evaluateParamExpressions.list = function(obj, dict = NULL) {
  assertClass(obj, "list")
  assertList(dict, names = "unique", null.ok = TRUE)
  ids = names(obj)
  # evaluate all parameter values separately
  setNames(lapply(obj, function(par) {
    eval(expr = par, envir = dict)
  }), ids)
}
