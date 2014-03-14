#' Check parameter set for discrete params.
#'
#' \code{TRUE} iff the parameter set contains at least one discrete parameter.
#'
#' @param par \code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return [\code{logical(1)}].
#' @export
hasDiscrete = function(par) {
  UseMethod("hasDiscrete")
}

#' @S3method hasDiscrete ParamSet
hasDiscrete.ParamSet = function(par) {
  return(hasSomeParamsOfTypes(par, types = c("discrete", "discretevector")))
}

#' Check parameter set for integer params.
#'
#' \code{TRUE} iff the parameter set contains at least one integer parameter.
#'
#' @param par \code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return [\code{logical(1)}].
#' @export
hasInteger = function(par) {
  UseMethod("hasInteger")
}

#' @S3method hasInteger ParamSet
hasInteger.ParamSet = function(par) {
  return(hasSomeParamsOfTypes(par, types = c("integer", "integervector")))
}

#' Check parameter set for numeric params.
#'
#' \code{TRUE} iff the parameter set contains at least one numeric parameter.
#'
#' @param par \code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return [\code{logical(1)}].
#' @export
hasNumeric = function(par) {
  UseMethod("hasNumeric")
}

#' @S3method hasNumeric ParamSet
hasNumeric.ParamSet = function(par) {
  return(hasSomeParamsOfTypes(par, types = c("numeric", "numericvector", "integer", "integervector")))
}
