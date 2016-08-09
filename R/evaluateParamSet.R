#' @title Evaluates expressions of a parameter set.
#'
#' @description
#' Evaluates the expressions of a parameter set for a given environment.
#'
#' @template arg_parset
#' @template arg_dict
#' @return [\code{TRUE}].
#' @export
#' @examples
#' # FIXME: example
evaluateParamSet = function(par.set, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  if (!hasExpression(par.set))
    return(par.set)
  checkExpressionFeasibility(par.set = par.set, keys = names(dict))
  # FIXME: vcapply?
  ids = as.character(unlist(lapply(par.set$pars, function(x) x$id)))
  # replace expressions in length (needs to be done prior to computing
  # defaults, values and boundaries)
  lengths = getParamLengths(par.set = par.set, dict = dict)
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
      par$lower = unname(getLower(par.set = ps, dict = dict))
    if (!is.null(par$upper))
      par$upper = unname(getUpper(par.set = ps, dict = dict))
    if (!is.null(par$default))
      par$default = getDefaults(par.set = ps, dict = dict)[[id]]
    if (!is.null(par$values))
      par$values = getValues(par.set = ps, dict = dict)[[id]]
    return(par)
  })
  names(par.set$pars) = ids
  return(par.set)
}
