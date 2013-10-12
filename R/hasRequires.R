#' Check parameter / parameter set for requirements / dependencies.
#' 
#' \code{TRUE} iff the parameter has any requirements or any parameter in the set has
#' requirements. 
#'
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @return [\code{logical(1)}].
#' @export
hasRequires = function(par) {
  UseMethod("hasRequires")
}

#' @S3method hasRequires Param
hasRequires.Param = function(par) {
  !is.null(par$requires)
}

#' @S3method hasRequires ParamSet
hasRequires.ParamSet = function(par) {
  any(vapply(par$pars, hasRequires, logical(1L)))
}

