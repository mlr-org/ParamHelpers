#' @title Get parameter set.
#'
#' @description
#' \code{getParamSet} is a generic and can be called to extract the \code{ParamSet} from different objects.
#'
#' @param x [\code{object}] \cr
#'   Object to extract the ParamSet from.
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @export
getParamSet = function(x) {
  UseMethod("getParamSet")
}
