#' @title Check if parameter values contain expressions.
#'
#' @description Check if parameter values contain expressions.
#' @template arg_par_or_set
#' @return [\code{logical(1)}].
#' @export
hasExpression = function(par) {
  UseMethod("hasExpression")
}

#' @export
hasExpression.Param = function(par) {
  return(any(vapply(par, is.expression, logical(1L))))
}

#' @export
hasExpression.LearnerParam = function(par) {
  return(any(vapply(par, is.expression, logical(1L))))
}

#' @export
hasExpression.ParamSet = function(par) {
  return(any(vapply(par$pars, hasExpression, logical(1L))))
}

#' @export
hasExpression.LearnerParamSet = function(par) {
  return(any(vapply(par$pars, hasExpression, logical(1L))))
}
