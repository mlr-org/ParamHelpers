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

#' @export
hasRequires.Param = function(par) {
  !is.null(par$requires)
}

#' @export
hasRequires.ParamSet = function(par) {
  any(vapply(par$pars, hasRequires, logical(1L)))
}

