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
#' @param envir [\code{list} | \code{NULL}]\cr
#'   Environment, which will be used for replacing the arguments
#'   of the expression within a parameter set. The default is \code{NULL}.
#' @return [named \code{list}]. Named and in same order as \code{par.set}.
#'   Parameters without defaults are not present in the list.
#' @export
getDefaults = function(par.set, include.null = FALSE, envir = NULL) {
  assertClass(par.set, "ParamSet")
  assertFlag(include.null)
  if (isEmpty(par.set))
    return(list())
  defs = extractSubList(par.set$pars, "default", simplify = FALSE)
  if (all(vapply(defs, is.null, logical(1L))))
    return(list())
  if (!include.null) {
    j = vlapply(par.set$pars, function(x) x$has.default)
    if (!any(j))
      return(list())
    ids = names(defs)[j]
    defs = lapply(ids, function(id) {
      def = defs[[id]]
      if (is.expression(def))
        def = eval(def, envir = envir)
      if ((length(def) == 1) && par.set$pars[[id]]$len > 1)
        rep(def, par.set$pars[[id]]$len)
      else
        def
    })
    names(defs) = ids
  }
  return(defs)
}
