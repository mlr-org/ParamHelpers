#' @title Check whether parameter set contains a certain type.
#'
#' @description
#' \code{TRUE} iff the parameter set contains at least one parameter of the mentioned type x.
#' Type x always subsumes x and x-vector.
#'
#' @template arg_parset
#' @template arg_include_int
#' @template arg_include_logical
#' @return [\code{logical(1)}].
#' @name hasType
#' @rdname hasType
NULL

#' @export
#' @rdname hasType
hasDiscrete = function(par.set, include.logical = TRUE) {
  assertClass(par.set, "ParamSet")
  hasSomeParamsOfTypes(par.set, types = getTypeStringsDiscrete(include.logical = include.logical))
}

#' @export
#' @rdname hasType
hasInteger = function(par.set) {
  assertClass(par.set, "ParamSet")
  return(hasSomeParamsOfTypes(par.set, types = c("integer", "integervector")))
}

#' @export
#' @rdname hasType
hasLogical = function(par.set) {
  assertClass(par.set, "ParamSet")
  return(hasSomeParamsOfTypes(par.set, types = c("logical", "logicalvector")))
}

#' @export
#' @rdname hasType
hasCharacter = function(par.set) {
  assertClass(par.set, "ParamSet")
  return(hasSomeParamsOfTypes(par.set, types = c("character", "charactervector")))
}

#' @export
#' @rdname hasType
hasNumeric = function(par.set, include.int = TRUE) {
  assertClass(par.set, "ParamSet")
  hasSomeParamsOfTypes(par.set, types = getTypeStringsNumeric(include.int = include.int))
}

##### helpers

# is at least one of types somewhere in par.set?
hasSomeParamsOfTypes = function(par.set, types) {
  return(any(types %in% getParamTypes(par.set, df.cols = FALSE, with.nr = FALSE , use.names = FALSE)))
}

# are all param types contained in 'types'
hasAllParamsOfTypes = function(par.set, types) {
  return(all(getParamTypes(par.set, df.cols = FALSE, with.nr = FALSE , use.names = FALSE) %in% types))
}

