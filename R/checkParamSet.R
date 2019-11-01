#' @title Sanity check expressions of a parameter set.
#'
#' @description
#' Checks whether the default values of the numerical parameters are located
#' within the corresponding boundaries. In case of discrete parameters it
#' checks whether the values are a subset of the parameter's possible values.
#'
#' @template arg_parset
#' @template arg_dict
#' @return [`TRUE`] on success. An exception is raised otherwise.
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

  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)

  # evaluate expressions of par.set (in case it contains any)
  if (hasExpression(par.set)) {
    if (is.null(dict)) { # error if dict is not defined, but par.set contains expressions
      stop("At least one of the parameters contains expressions and therefore 'dict' has to be defined.")
    }
    par.set = evaluateParamExpressions(obj = par.set, dict = dict)
  }

  # extract bounds, default and values (dict = NULL is sufficient) because
  # the par.set is already evaluated
  lower = getLower(par.set, dict = NULL)
  upper = getUpper(par.set, dict = NULL)
  default = getDefaults(par.set, dict = NULL)
  values = getValues(par.set, dict = NULL)

  # check if defaults are outside the bounds
  failed.boundary.check = which(vlapply(seq_along(lower), function(i) {
    id = names(lower)[i]
    any(lower[[id]] > default[[id]]) || any(upper[[id]] < default[[id]])
  }))
  if (length(failed.boundary.check) > 0L) {
    stopf("The following %s failed the boundary check: %s",
      ifelse(length(failed.boundary.check) > 1L, "parameters", "parameter"),
      paste(names(failed.boundary.check), collapse = ", "))
  }

  # check if defaults are outside the feasible values
  failed.value.check = which(vlapply(seq_along(default), function(i) {
    id = names(default)[i]
    !is.null(values[[id]]) && !is.null(default[[id]]) && !(default[[id]] %in% values[[id]])
  }))
  if (length(failed.value.check) > 0L) {
    stopf("The following %s 'defaults' that are not part of the possible 'values': %s",
      ifelse(length(failed.boundary.check) > 1L, "parameters have", "parameter has"),
      paste(names(failed.boundary.check), collapse = ", "))
  }

  return(TRUE)
}

# check whether the expressions of a param set are feasible
# (internal function which is called in case 'keys' are provided
# at the construction of the par.set)
checkExpressionFeasibility = function(par.set, keys) {
  has.expression = vlapply(par.set$pars, hasExpression)
  # if par.set has no expressions, the expressions are feasible
  if (!any(has.expression)) {
    return(TRUE)
  }

  # for all params which have an expression, check whether its
  # arguments are listed in 'keys'
  lapply(par.set$pars[has.expression], function(par) {
    # extract all parameter arguments that have expressions
    expressions = par[vlapply(par, is.expression)]
    lapply(expressions, function(expr) {
      # arguments from expression that are not defined in keys
      missing.vars = all.vars(expr)[all.vars(expr) %nin% keys]
      if (length(missing.vars) > 0L) {
        message = sprintf("The %s '%s' %s to be defined in 'keys'",
          ifelse(length(missing.vars) == 1L, "parameter", "parameters"),
          paste(missing.vars, collapse = "', '"),
          ifelse(length(missing.vars) == 1L, "needs", "need"))
        stop(message)
      }
    })
  })

  return(TRUE)
}
