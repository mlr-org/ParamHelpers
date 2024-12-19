# @description
#   Returns the names of all params which are required by the given Param or ParamSet.
# @template arg_par_or_set
# @return [\code{character}]: names of parameters which are required
getRequiredParamNames = function(par) {
  UseMethod("getRequiredParamNames")
}

#' @export
getRequiredParamNames.Param = function(par) {
  all.vars(par$requires)
}

#' @export
getRequiredParamNames.ParamSet = function(par) {
  if (isEmpty(par)) {
    return(character(0L))
  }
  unique(unlist(lapply(par$pars, getRequiredParamNames.Param), use.names = FALSE))
}
