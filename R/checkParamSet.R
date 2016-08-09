#' @title Sanity check expressions of a parameter set.
#'
#' @description
#' Checks whether the default values of the numerical parameters are located
#' within the corresponding boundaries. In case of discrete parameters it
#' checks whether the values are a subset of the parameter's possible values.
#'
#' @template arg_parset
#' @template arg_dict
#' @return [\code{TRUE}].
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u", lower = expression(p)),
#'   makeIntegerParam("v", lower = 1, upper = expression(3 * p)),
#'   makeDiscreteParam("w", default = expression(z), values = c("a", "b")),
#'   makeDiscreteParam("x", default = "a", values = c("a", "b")),
#'   keys = c("p", "z")
#' )
#' checkParamSet(ps, dict = list(p = 3, z = "b"))
checkParamSet = function(par.set, dict = NULL) {
  if (hasExpression(par.set) && is.null(dict))
    stop("At least one of the parameters contains expressions and therefore 'dict' has to be defined.")
  if (hasExpression(par.set))
    par.set = evaluateParamSet(par.set = par.set, dict = dict)
  lower = getLower(par.set, dict = dict)
  upper = getUpper(par.set, dict = dict)
  default = getDefaults(par.set, dict = dict)
  values = getValues(par.set, dict = envir)
  failed.boundary.check = vlapply(seq_along(lower), function(i) {
    id = names(lower)[i]
    any(lower[[id]] > default[[id]]) || any(upper[[id]] < default[[id]])
  })
  if (any(failed.boundary.check)) {
    stopf("The following %s failed the boundary check: %s",
      ifelse(sum(failed.boundary.check) > 1, "parameters", "parameter"),
      paste(names(which(failed.boundary.check)), collapse = ", "))
  }
  failed.value.check = vlapply(seq_along(default), function(i) {
    id = names(default)[i]
    !is.null(values[[id]]) && !is.null(default[[id]]) && !(default[[id]] %in% values[[id]])
  })
  if (any(failed.value.check)) {
    stopf("The following %s 'defaults' that are not part of the possible 'values': %s",
      ifelse(sum(failed.boundary.check) > 1L, "parameters have", "parameter has"),
      paste(names(which(failed.boundary.check)), collapse = ", "))
  }
  return(TRUE)
}

# check whether the expressions of a param set are feasible
checkExpressionFeasibility = function(par.set, keys) {
  expression.flag = vlapply(par.set$pars, hasExpression)
  if (sum(expression.flag) == 0L)
    return(TRUE)
  lapply(par.set$pars[expression.flag], function(par) {
    id = par$id
    expressions = par[vlapply(par, is.expression)]
    lapply(expressions, function(expr) {
      missing.vars = all.vars(expr)[all.vars(expr) %nin% keys]
      if (length(missing.vars) > 0L) {
        res = sprintf("The %s '%s' %s to be defined in 'keys'",
          ifelse(length(missing.vars) == 1L, "parameter", "parameters"),
            paste(missing.vars, collapse = "', '"),
            ifelse(length(missing.vars) == 1L, "needs", "need"))
        # FIXME: makeAssertion?
        makeAssertion(id, res = res, collection = NULL)
      }
    })
  })
  return(TRUE)
}
