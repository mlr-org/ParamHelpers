#' Returns the default settings for each parameter
#'
#' @template arg_parset
#' @return [\code{list}]
#'  Named list which contains the default value for each parameter in the ParamSet.
#'  If no default value exists the given list item will contain \code{NULL}.
#' @export

getParamDefaultVals = function(par.set) {
	assertClass(par.set, "ParamSet")
	mapply(function(id) par.set$pars[[id]]$default, getParamIds(par.set))
}