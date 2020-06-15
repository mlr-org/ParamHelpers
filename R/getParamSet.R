#' @title Get parameter set.
#'
#' @description `getParamSet` is a generic and can be called to extract the
#' `ParamSet` from different objects.
#'
#' @param x (`object`) \cr
#'   Object to extract the ParamSet from.
#' @return `ParamHelpers::ParamSet()`
#' @export
getParamSet = function(x) {
  UseMethod("getParamSet")
}
