#' Removes all scalar NAs from a parameter setting list.
#' 
#' @param x [\code{list}]\cr
#'   List of paramter values.
#' @return [\code{list}].
removeMissingValues = function(x) {
  Filter(Negate(isScalarNA), x)
}