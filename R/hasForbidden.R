#' Check parameter set for forbidden region.
#'
#' @template parset
#' @return [\code{logical(1)}].
#' @export
hasForbidden = function(par.set) {
  !is.null(par.set$forbidden)
}


