# @description
#   Returns the names of all params which are required but not on the given par.vals
# @template arg_par_or_set
# @param par.val.names [\code(character)] \cr
#   names of the available parameter values.
# @return [\code{character}]: names of missing parameters
getMissingRequiredParams = function(par, par.val.names) {
  UseMethod("getMissingRequiredParams")
}

getMissingRequiredParams.Param = function(par, par.val.names) {
  req.vars = all.vars(par$requires)
  req.vars[req.vars %nin% par.val.names]
}

getMissingRequiredParams.ParamSet = function(par, par.val.names) {
  if (length(par$pars) == 0L || length(par.val.names) == 0L)
    return(character(0L))
  unique(unlist(lapply(par$pars, getMissingRequiredParams.Param, par.val.names = par.val.names), use.names = FALSE))
}
