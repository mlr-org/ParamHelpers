#' @title Check if a param value is technically valid.
#'
#' @description
#' This is not a check for feasibility, e.g. checking of constraints, but whether parameters
#' have the correct names and data types, so that the parameter setting technically makes sense
#' for a \code{ParamSet}.
#'
#' @template arg_parset
#' @param x [any] \cr
#'   Single value to check.
#'   For a parameter set this must be a list.
#' @return Nothing. The function produces a meaningful exception
#'   if conditions are violated.
#' @export
checkParVals = function(ps, x) {
  assertList(x)
  named = testNamed(x)
  ns1 = names(par$pars)
  ns2 = names(x)
  # if we have names, check that they are ok
  if (named)
    d = setdiff(ns2, ns2)
    if (length(d) > 0L)
      stopf("'x' used invalid names which are not in param set 'ps': %s", collapse(d))
  # check length of x
  if (length(x) != length(par$pars) {
    stopf("Length of 'x' is %i, but must be equal to number
      of params in 'ps': %i", length(x), length(par$pars))
  invisible(NULL)
}
