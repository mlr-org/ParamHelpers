#' @title Return defaults of parameters in parameter set.
#'
#' @description
#' Return defaults of parameters in parameter set.
#'
#' @template arg_parset
#' @param include.null [\code{logical(1)}]\cr
#'   Include \code{NULL} entries for parameters without default values in the result list?
#'   Note that this can be slightly dangerous as \code{NULL} might be used as default value
#'   for other parameters.
#'   Default is \code{FALSE}.
#' @template arg_dict
#' @return [named \code{list}]. Named and in same order as \code{par.set}.
#'   Parameters without defaults are not present in the list.
#' @export
getDefaults = function(par.set, include.null = FALSE, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertFlag(include.null)
  assertList(dict, names = "unique", null.ok = TRUE)
  if (isEmpty(par.set))
    return(list())
  defs = extractSubList(par.set$pars, "default", simplify = FALSE)
  if (all(vlapply(defs, is.null)))
    return(list())
  if (!include.null) {
    j = vlapply(par.set$pars, function(x) x$has.default)
    if (!any(j))
      return(list())
    ids = names(defs)[j]
    defs = setNames(lapply(ids, function(id) {
      def = defs[[id]]
      if (is.expression(def))
        def = eval(def, envir = dict)
      if ((length(def) == 1L) && !is.na(par.set$pars[[id]]$len) && par.set$pars[[id]]$len > 1L)
        def = rep(def, par.set$pars[[id]]$len)
      return(def)
    }), ids)
  }
  return(defs)
}
