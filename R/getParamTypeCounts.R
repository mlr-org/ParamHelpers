#' Returns information on the number of parameters of a each type.
#'
#' @param par.set [\code{link{ParamSet}}]\cr
#'  Parameter set.
#' @return [\code{list}]
#'  Named list which contains for each supported parameter type the
#'  number of parameters of this type in the given ParamSet.
#' @export
getParamTypeCounts = function(par.set) {
  checkArg(par.set, "ParamSet")
  supported.types = getSupportedParamTypes()
  par.types = getTypes(par.set)
  count = lapply(par.types, function(type) {
    sum(par.types == type)
  })
  names(count) = supported.types
  return(count)
}

# Returns a vector of supported parameter types.
getSupportedParamTypes = function() {
  return(
    c("numeric", "numericvector",
      "integer", "integervector",
      "discrete", "discretevector",
      "logical", "logicalvector",
      "function",
      "untyped"))
}
