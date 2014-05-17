#' Extracts ParamSet from soobench function.
#'
#' @param fn [\code{\link[soobench]{soo_function}}]\cr
#'   Source function.
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#'   Parameter set of type \code{\link[ParamHelpers]{ParamSet}}.
#' @examples
#' library(soobench)
#' fn = ackley_function(4)
#' par.set = extractParamSetFromSooFunction(fn)
#' print(par.set)
#' @export
extractParamSetFromSooFunction = function(fn) {
  checkArg(fn, "soo_function")
  makeNumericParamSet(
    len = number_of_parameters(fn),
    id = "x",
    lower = lower_bounds(fn),
    upper = upper_bounds(fn),
    vector = FALSE
  )
}
