#' @title Return all require-expressions of a param set.
#'
#' @description
#' Returns all require-expressions of a param set as a list.
#' You can filter for certain param types as \code{\link{filterParams}} is called internally.
#' And select whether params without a requires-setting result in a \code{NULL} element in the returned
#' list or not.
#'
#' @template arg_parset
#' @param ... [any]\cr
#'   Passed down to \code{\link{filterParams}},
#'   only selected params will create an entry in the returned list.
#' @param remove.null [\code{logical(1)}]\cr
#'   If not set, params without a requires-setting will result in a \code{NULL} element in the returned list,
#'   otherwise they are removed.
#'   Default is code{TRUE}.
#' @return [named \code{list}].
#'   Named list of require-expressions, lengths corresponds to number of filtered params (potentially
#'   only the subset with requires-field), named with with param ids.
#' @export
getRequirements = function(par.set, ..., remove.null = TRUE) {
  assertFlag(remove.null)
  # asserts are handled in next call
  ps2 = filterParams(par.set, ...)
  res = extractSubList(ps2$pars, "requires", simplify = FALSE, use.names = TRUE)
  if (remove.null)
    res = Filter(Negate(is.null), res)
  return(res)
}

