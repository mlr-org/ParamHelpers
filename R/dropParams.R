#' Drop Params from ParamSet by ids.
#'
#' @template arg_parset
#' @param drop (`character`)\cr
#'   `id`s of the [Param()]s in the [ParamSet()] to drop from the ParamSet.
#' @return [[ParamSet()]].
#' @export
dropParams = function(par.set, drop) {
  assertClass(par.set, "ParamSet")
  assertSubset(drop, getParamIds(par.set))
  par.set$pars = Filter(function(p) p$id %nin% drop, par.set$pars)
  return(par.set)
}
