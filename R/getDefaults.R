#' Return defaults of parameters in parameter set.
#'
#' @template arg_parset
#' @return [named \code{list}]. Named and in same order as \code{par.set}.
#'   Parameters without defaults are not present in the list.
#' @export
getDefaults = function(par.set) {
  assertClass(par.set, "ParamSet")
  if (isEmpty(par.set))
    return(list())
  j = vlapply(par.set$pars, function(x) x$has.default)
  if (!any(j))
    return(list())
  defs = extractSubList(par.set$pars, "default", simplify = FALSE)
  return(defs[j])
}

