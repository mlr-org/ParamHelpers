#' @title Is a given value in the list of special values for a param?
#'
#' @description
#' See title.
#'
#' @template arg_par
#' @param x [any] \cr
#'   Single value to check.
#' @return [\code{logical(1)}].
#' @export
isSpecialValue = function(par, x) {
  any(vlapply(par$special.vals, function(special.val) isTRUE(all.equal(x, special.val))))
}

