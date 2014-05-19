#' Check parameter set for forbidden region.
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return [\code{logical(1)}].
#' @export
hasForbidden = function(par.set) {
  !is.null(par.set$forbidden)
}


