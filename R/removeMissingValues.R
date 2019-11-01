#' Removes all scalar NAs from a parameter setting list.
#'
#' @param x [list]\cr
#'   List of parameter values.
#' @return [list].
#' @export
removeMissingValues = function(x) {
  return(Filter(Negate(isScalarNA), x))
}
