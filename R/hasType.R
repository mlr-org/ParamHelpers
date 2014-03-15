#' Check whether parameter set contains a certain type.
#'
#' \code{TRUE} iff the parameter set contains at least one parameter of the mentioned type x.
#' Type x always subsumes x and x-vector.
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param include.int [\code{logical(1)}]\cr
#'   Are integers also considered as numeric?
#'   Default is \code{TRUE}.
#' @return [\code{logical(1)}].
#' @name hasType
#' @rdname hasType
NULL

#' @export
#' @rdname hasType
hasDiscrete = function(par.set) {
  checkArg(par.set, "ParamSet")
  return(hasSomeParamsOfTypes(par.set, types = c("discrete", "discretevector")))
}

#' @export
#' @rdname hasType
hasInteger = function(par.set) {
  checkArg(par.set, "ParamSet")
  return(hasSomeParamsOfTypes(par.set, types = c("integer", "integervector")))
}

#' @export
#' @rdname hasType
hasNumeric = function(par.set, include.int = TRUE) {
  checkArg(par.set, "ParamSet")
  types = if (include.int)
    c("numeric", "numericvector", "integer", "integervector")
  else
    c("numeric", "numericvector")
  return(hasSomeParamsOfTypes(par.set, types = types))
}

##### helpers
# FIXME: do we really need to consider the empty parset for the next functons?

# is at least one of types somewhere in par.set?
hasSomeParamsOfTypes = function(par.set, types) {
  any(par.types %in% getTypes(par.set))
}

# are all param types contained in 'types'
hasAllParamsOfTypes = function(par.set, types) {
  all(par.types %in% getTypes(par.set))
}


