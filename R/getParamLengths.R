#' @title Return lengths of parameters in parameter set.
#'
#' @description
#' Useful for vector parameters.
#'
#' @template arg_parset
#' @template arg_dict
#' @return [\code{integer}]. Named and in same order as \code{par.set}.
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeDiscreteVectorParam("x", len = 2, values = c("a", "b"))
#' )
#' getParamLengths(ps)
#' # the length of the vector x is 2, for all other single value parameters the length is 1.
#' @export
getParamLengths = function(par.set, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  if (isEmpty(par.set))
    return(integer(0L))

  lengths = extractSubList(par.set$pars, "len", simplify = FALSE)
  j = vlapply(par.set$pars, function(x) is.expression(x$len))
  lengths[j] = lapply(lengths[j], eval, envir = dict)
  setNames(as.integer(lengths), names(lengths))
}
