#' Returns information on the number of parameters of a each type.
#'
#' @template arg_parset
#' @return [`list`]
#'  Named list which contains for each supported parameter type the
#'  number of parameters of this type in the given ParamSet.
#' @export
getParamTypeCounts = function(par.set) {
  assertClass(par.set, "ParamSet")
  supported.types = getTypeStringsAll()
  par.types = getParamTypes(par.set)
  count = lapply(supported.types, function(type) {
    sum(par.types == type)
  })
  names(count) = supported.types
  return(count)
}
