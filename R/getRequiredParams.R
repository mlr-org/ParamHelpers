#' @title Return all required params in a param set. 
#'
#' @description 
#' Returns all params which are required by the given [\code{\link{ParamSet}}].
#' If no parameter is required in the param set an empty param set is returned. 
#'
#' @template arg_parset
#' @return par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @export
getRequiredParams = function(par.set) {
  reqs = getRequirements(par.set)
  # find all parameters that are in require in the param set at least once
  all.reqs = unique(unlist(lapply(reqs, all.vars)))
  # remove required parameters that are not in the par.set 
  all.reqs = intersect(getParamIds(par.set), all.reqs)
  
  # if no parameters are in require return an empty parameter set
  if (is.null(all.reqs))
    makeParamSet()
  else
    filterParams(par.set, ids = all.reqs)
}
