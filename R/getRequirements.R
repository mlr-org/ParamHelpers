#' @title Return all require-expressions of a param set.
#'
#' @description
#' Returns all `require`s-objects of a param set as a list.
#'
#' @template arg_parset
#' @param remove.null (`logical(1)`)\cr
#'   If not set, params without a requires-setting will result in a `NULL`
#'   element in the returned list, otherwise they are removed. Default is
#'   \code{TRUE}.
#' @return xnamed `list`.
#'   Named list of require-call-objects, lengths corresponds to number of params
#'   (potentially only the subset with requires-field), named with with param
#'   ids.
#' @export
getRequirements = function(par.set, remove.null = TRUE) {
  assertClass(par.set, "ParamSet")
  assertFlag(remove.null)
  res = extractSubList(par.set$pars, "requires", simplify = FALSE, use.names = TRUE)
  if (remove.null) {
    res = filterNull(res)
  }
  return(res)
}
