#' Check parameters for requirements / dependencies.
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return Named logical vector with TRUE for parameters with requirements.
#' @export
hasRequires = function(par.set) {
  checkArg(par.set, "ParamSet")
  vapply(par.set$pars, function(x) is.null(x$requires), logical(1L))
}
