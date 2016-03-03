#' @title Evaluates expressions of a parameter set.
#'
#' @description
#' Evaluates the expressions of a parameter set for a given environment.
#'
#' @template arg_parset
#' @param envir [\code{list} | \code{NULL}]\cr
#'   If any of the parameters components is defined with an expression, this
#'   object will be used to replace the values within the expressions.
#'   The default is \code{NULL}.
#' @return [\code{TRUE}].
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u", lower=1),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeLogicalParam("x"),
#'   makeDiscreteVectorParam("y", len=2, values=c("a", "b"))
#' )
evaluateParamSet = function(par.set, envir = NULL) {
  if (!hasExpression(par.set))
    return(par.set)
  checkExpressionFeasibility(par.set = par.set, dictionary = names(envir))
  ids = as.character(unlist(lapply(par.set$pars, function(x) x$id)))
  # replace expressions in length (needs to be done prior to computing
  # defaults, values and boundaries)
  lengths = getParamLengths(par.set = par.set, envir = envir)
  par.set$pars = lapply(ids, function(id) {
    par = par.set$pars[[id]]
    par$len = asInt(lengths[[id]])
    return(par)
  })
  names(par.set$pars) = ids
  # replace expressions in default, values and boundaries
  par.set$pars = lapply(ids, function(id) {
    ps = par.set
    ps$pars = ps$pars[id]
    par = ps$pars[[id]]
    if (!is.null(par$lower))
      par$lower = unname(getLower(par.set = ps, envir = envir))
    if (!is.null(par$upper))
      par$upper = unname(getUpper(par.set = ps, envir = envir))
    if (!is.null(par$default))
      par$default = getDefaults(par.set = ps, envir = envir)[[id]]
    if (!is.null(par$values))
      par$values = getValues(par.set = ps, envir = envir)[[id]]
    return(par)
  })
  names(par.set$pars) = ids
  return(par.set)
}
