#' Subset Params from ParamSet by ids.
#'
#' @template arg_parset
#' @param subset [\code{character}]\cr
#'   \code{id}s of the \code{\link{Param}}s in the \code{\link{ParamSet}} to keep from the ParamSet.
#' @return [\code{\link{ParamSet}}].
#' @export
subsetParams = function(par.set, subset) {
  assertClass(par.set, "ParamSet")
  assertSubset(subset, getParamIds(par.set))
  par.set$pars = Filter(function(p) p$id %in% subset, par.set$pars)
  return(par.set)
}
