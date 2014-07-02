#' Check whether parameter set contains a certain type.
#'
#' \code{TRUE} iff the parameter set contains at least one parameter of the mentioned type x.
#' Type x always subsumes x and x-vector.
#'
#' @template arg_parset
#' @param include.int [\code{logical(1)}]\cr
#'   Are integers also considered to be numeric?
#'   Default is \code{TRUE}.
#' @return [\code{logical(1)}].
#' @name hasType
#' @rdname hasType
NULL

#' @export
#' @rdname hasType
hasDiscrete = function(par.set) {
  assertClass(par.set, "ParamSet")
  return(hasSomeParamsOfTypes(par.set, types = c("discrete", "discretevector")))
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
hasNumeric = function(par.set, include.int = TRUE) {
  assertClass(par.set, "ParamSet")
  types = if (include.int)
    c("numeric", "numericvector", "integer", "integervector")
  else
    c("numeric", "numericvector")
  return(hasSomeParamsOfTypes(par.set, types = types))
}

##### helpers

# is at least one of types somewhere in par.set?
hasSomeParamsOfTypes = function(par.set, types) {
  any(types %in% getParamTypes(par.set, df.cols = FALSE, with.nr = FALSE , use.names = FALSE))
}

# are all param types contained in 'types'
hasAllParamsOfTypes = function(par.set, types) {
  all(getParamTypes(par.set, df.cols = FALSE, with.nr = FALSE , use.names = FALSE) %in% types)
}
