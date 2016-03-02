#' @title Extract a ParamSet.
#' @description
#'  This is a generic function used to extract a \code{\link{ParamSet}} out of
#'  different objects, e.g. an \code{\link{OptPath}}.
#'
#' @param x [any]\cr
#' @return \code{\link{ParamSet}}
#' @export
getParamSet = function(x, ...) {
  UseMethod("getParamSet")
}

#' @export
getParamSet.ParamSet = function(x, ...) {
  x
}

#' @export
getParamSet.OptPath = function(x, ...) {
  x$par.set
}
