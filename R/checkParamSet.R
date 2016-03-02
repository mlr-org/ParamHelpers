#' @title Sanity check expressions of a parameter set.
#'
#' @description
#' Checks whether the default values of the numerical parameters are located
#' within the corresponding boundaries. In case of discrete parameters it
#' checks whether the values are a subset of the parameter's possible values.
#'
#' @template arg_parset
#' @param envir [\code{list} | \code{NULL}]\cr
#'   If any of the parameters components is defined with an expression, this
#'   list will be used to replace the values within the expressions.
#'   The default is \code{NULL}.
#' @return [\code{TRUE}].
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u", lower = expression(p)),
#'   makeIntegerParam("v", lower = 1, upper = expression(3 * p)),
#'   makeDiscreteParam("w", default = expression(z), values = c("a", "b")),
#'   makeDiscreteParam("x", default = "a", values = c("a", "b")),
#'   dictionary = c("p", "z")
#' )
#' checkParamSet(ps, envir = list(p = 3, z = "b"))
checkParamSet = function(par.set, envir = NULL) {
  if (hasExpression(par.set) && is.null(envir))
    stop("At least one of the parameters contains expressions and therefore 'envir' has to be defined.")
  if (hasExpression(par.set))
    par.set = evaluateParamSet(par.set = par.set, envir = envir)
  lower = getLower(par.set, envir = envir)
  upper = getUpper(par.set, envir = envir)
  default = getDefaults(par.set, envir = envir)
  values = getValues(par.set, envir = envir)
  failed.boundary.check = vapply(seq_along(lower), function(i) {
    id = names(lower)[i]
    any(lower[[id]] > default[[id]]) || any(upper[[id]] < default[[id]])
  }, logical(1L))
  if (any(failed.boundary.check)) {
    stopf("The following %s failed the boundary check: %s",
      ifelse(sum(failed.boundary.check) > 1, "parameters", "parameter"),
      paste(names(which(failed.boundary.check)), collapse = ", "))
  }
  failed.value.check = vapply(seq_along(default), function(i) {
    id = names(default)[i]
    !is.null(values[[id]]) && !is.null(default[[id]]) && !(default[[id]] %in% values[[id]])
  }, logical(1L))
  if (any(failed.value.check)) {
    stopf("The following %s 'defaults' that are not part of the possible 'values': %s",
      ifelse(sum(failed.boundary.check) > 1, "parameters have", "parameter has"),
      paste(names(which(failed.boundary.check)), collapse = ", "))
  }
  return(TRUE)
}

# check whether the expressions of a param set are feasible
checkExpressionFeasibility = function(par.set, dictionary) {
  expression.flag = vapply(par.set$pars, hasExpression, logical(1L))
  if (sum(expression.flag) == 0)
    return(TRUE)
  lapply(par.set$pars[expression.flag], function(par) {
    id = par$id
    expressions = par[vapply(par, is.expression, logical(1L))]
    lapply(expressions, function(expr) {
      missing.vars = all.vars(expr)[all.vars(expr) %nin% dictionary]
      if (length(missing.vars) > 0) {
        res = sprintf("The %s '%s' %s to be defined in 'dictionary'.",
          ifelse(length(missing.vars) == 1, "parameter", "parameters"),
            paste(missing.vars, collapse = "', '"),
            ifelse(length(missing.vars) == 1, "needs", "need"))
        makeAssertion(id, res = res, collection = NULL)
      }  
    })
  })
  return(TRUE)
}
