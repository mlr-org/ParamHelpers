#' Check parameter / parameter set for vector params.
#'
#' \code{TRUE} iff the parameter is a vector parameter or all parameters in the
#' set are vector parameters.
#'
#' @template arg_par_or_set
#' @return [\code{logical(1)}].
#' @export
isVector = function(par) {
  UseMethod("isVector")
}

#' @export
isVector.Param = function(par) {
  return(grepl("vector", fixed = TRUE, par$type))
}

#' @export
isVector.ParamSet = function(par) {
  return(all(vapply(par$pars, isVector, logical(1L))))
}

