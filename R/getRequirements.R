#' @title Return all require-expressions of a param set.
#'
#' @description
#' Returns all \code{require}s-objects of a param set as a list.
#'
#' @template arg_parset
#' @param no.conditions [any] \cr
#'   Params without a requires-setting will result in this value in the returned list.
#'   Default is \dQuote{remove} which means to remove them from the list.
#' @return [named \code{list}].
#'   Named list of require-call-objects, lengths corresponds to number of params (potentially
#'   only the subset with requires-field), named with with param ids.
#' @export
getRequirements = function(par.set, no.conditions = "remove") {
  assertClass(par.set, "ParamSet")
  nocval = no.conditions
  if (identical(no.conditions, "remove"))
    nocval = NULL
  res = lapply(par.set$pars, function(p) {
    if (is.null(p$requires))
      return(nocval)
    else
      return(p$requires)
  })
  names(res) = names(par.set$pars)
  if (identical(no.conditions, "remove"))
    res = filterNull(res)
  return(res)
}
