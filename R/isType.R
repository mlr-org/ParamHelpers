#' Check parameter / parameter set contain ONLY a certain type.
#'
#' An empty param set is considered to be of all types.
#'
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @param include.int [\code{logical(1)}]\cr
#'   Are integers also considered to be numeric?
#'   Default is \code{TRUE}.
#' @return [\code{logical(1)}].
#' @name isType
#' @rdname isType
NULL

#' @export
#' @rdname isType
isNumeric = function(par, include.int = TRUE) {
  checkArg(par, c("Param", "ParamSet"))
  UseMethod("isNumeric")
}

#' @S3method isNumeric ParamSet
isNumeric.ParamSet = function(par, include.int = TRUE) {
  all(sapply(par$pars, isNumeric.Param, include.int = include.int))
}

#' @S3method isNumeric Param
isNumeric.Param = function(par, include.int = TRUE) {
  types = if (include.int)
    c("numeric", "numericvector", "integer", "integervector")
  else
    c("numeric", "numericvector")
  return(par$type %in% types)
}

#' @export
#' @rdname isType
isDiscrete = function(par) {
  checkArg(par, c("Param", "ParamSet"))
  UseMethod("isDiscrete")
}

#' @S3method isDiscrete ParamSet
isDiscrete.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("discrete", "discretevector")))
}

#' @S3method isDiscrete Param
isDiscrete.Param = function(par) {
  return(par$type %in% c("discrete", "discretevector"))
}

#' @export
#' @rdname isType
isInteger = function(par) {
  checkArg(par, c("Param", "ParamSet"))
  UseMethod("isInteger")
}

#' @S3method isInteger ParamSet
isInteger.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("integer", "integervector")))
}

#' @S3method isInteger Param
isInteger.Param = function(par) {
  return(par$type %in% c("integer", "integervector"))
}

