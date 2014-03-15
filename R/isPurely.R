#' Check parameter / parameter set for discrete params.
#'
#' \code{TRUE} iff the parameter is discrete or all parameters in the set are
#' discrete.
#'
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @return [\code{logical(1)}].
#' @export
isPurelyDiscrete = function(par) {
  UseMethod("isPurelyDiscrete")
}

#' @S3method isPurelyDiscrete ParamSet
isPurelyDiscrete.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("discrete", "discretevector")))
}

#' @S3method isPurelyDiscrete Param
isPurelyDiscrete.Param = function(par) {
  return(hasAllParamsOfTypes(par, types = c("discrete", "discretevector")))
}

#' Check parameter / parameter set for integer params.
#'
#' \code{TRUE} iff the parameter is of type integer or all parameters in the set are
#' integer parameters.
#'
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @return [\code{logical(1)}].
#' @export
isPurelyInteger = function(par) {
    UseMethod("isPurelyInteger")
}

#' @S3method isPurelyInteger ParamSet
isPurelyInteger.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("integer", "integervector")))
}

#' @S3method isPurelyInteger Param
isPurelyInteger.Param = function(par) {
  return(hasAllParamsOfTypes(par, types = c("integer", "integervector")))
}

#' Check parameter / parameter set for numeric params.
#'
#' \code{TRUE} iff the parameter is of type numeric or all parameters in the set are
#' numeric parameters.
#'
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @return [\code{logical(1)}].
#' @export
isPurelyNumeric = function(par) {
  return(hasAllParamsOfTypes(par, types = c("numeric", "numericvector")))
}

