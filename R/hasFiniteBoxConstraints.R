#' Checks if a parameter or each parameter of a parameter set has ONLY finite lower and upper bounds.
#'
#' @template arg_par_or_set
#' @template arg_dict
#' @return [\code{logical(1)}]
#' @export
hasFiniteBoxConstraints = function(par, dict = NULL) {
  UseMethod("hasFiniteBoxConstraints")
}

#' @export
hasFiniteBoxConstraints.Param = function(par, dict = NULL) {
  bounds = c(getLower(par, dict = dict), getUpper(par, dict = dict))
  if (length(bounds) == 0)
    return(TRUE)
  return(all(is.finite(bounds)))
}

#' @export
hasFiniteBoxConstraints.ParamSet = function(par, dict = NULL) {
  bounds = c(getLower(par, dict = dict), getUpper(par, dict = dict))
  if (length(bounds) == 0)
    return(TRUE)
  return(all(is.finite(bounds)))
}
