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
  any(vlapply(par, is.expression))
}

#' @export
hasExpression.LearnerParam = function(par) {
  any(vlapply(par, is.expression))
}

#' @export
hasExpression.ParamSet = function(par) {
  any(vlapply(par$pars, hasExpression))
}

#' @export
hasExpression.LearnerParamSet = function(par) {
  any(vlapply(par$pars, hasExpression))
}
