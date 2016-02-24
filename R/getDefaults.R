#' @title Return defaults of parameters in parameter set.
#'
#' @description
#' Return defaults of parameters in parameter set.
#'
#' @template arg_parset
#' @param include.null [\code{logical(1)}]\cr
#'   Include \code{NULL} entries for parameters without default values in the result list?
#'   Note that this can be slightly dangerous as \code{NULL} might be used as default value
#'   for other parameters.
#'   Default is \code{FALSE}.
#' @param envir [\code{list} | \code{NULL}]\cr
#'   Environment, which will be used as "dictionary" for replacing the arguments
#'   of the expression. The default is \code{NULL}.
#' @return [named \code{list}]. Named and in same order as \code{par.set}.
#'   Parameters without defaults are not present in the list.
#' @export
getDefaults = function(par.set, include.null = FALSE, envir = NULL) {
  assertClass(par.set, "ParamSet")
  assertFlag(include.null)
  if (isEmpty(par.set))
    return(list())
  defs = extractSubList(par.set$pars, "default", simplify = FALSE)
  if (!include.null) {
    j = vlapply(par.set$pars, function(x) x$has.default)
    if (!any(j))
      return(list())
    defs = defs[j]
    sapply(names(defs), function(id)
      checkExpressionFeasibility(id = id, expr = defs[[id]], envir = envir))
    defs = lapply(defs, eval, envir = envir)
  }
  return(defs)
}

