#' Repairs points outside of box constraints by clipping it to bounds.
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param x [\code{list}]\cr
#'   List of values which eventually are located outside of the bounds.
#' @param warn [\code{logical(1)}]\cr
#'   Boolean indicating whether a warning should be printed each time 
#'   a value is being repaired. Default is \code{FALSE}.
#' @return [\code{list}]:
#'   List of repaired points.
#' @export
repairPoint = function(par.set, x, warn = FALSE) {
  assertClass(par.set, "ParamSet")
  assertList(x)
  assertFlag(warn)
  Map(function(par, val) {
    if (isNumeric(par)) {
      if (!(length(val) == 1L && is.na(val)) && any(val < par$lower | val > par$upper)) {
        if (warn) {
          warningf("Repairing value for %s: %s", par$id, as.character(val))
        }
        val = pmax(par$lower, val)
        val = pmin(par$upper, val)
      }
    }
    return(val)
  }, par.set$pars, x)
}
