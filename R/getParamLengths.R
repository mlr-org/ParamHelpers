#' @title Return lengths of single parameters or parameters in parameter set.
#'
#' @description
#' Useful for vector parameters.
#'
#' @template arg_par_or_set
#' @template arg_dict
#' @return [\code{integer}]. Named (and in case of a \code{\link{ParamSet}}, in the same order).
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeDiscreteVectorParam("x", len = 2, values = c("a", "b"))
#' )
#' getParamLengths(ps)
#' # the length of the vector x is 2, for all other single value parameters the length is 1.
#'
#' par = makeNumericVectorParam("x", len = expression(k), lower = 0)
#' getParamLengths(par, dict = list(k = 4))
#' @export
getParamLengths = function(par, dict = NULL) {
  UseMethod("getParamLengths")
}

#' @export
getParamLengths.Param = function(par, dict = NULL) {
  assertClass(par, "Param")
  assertList(dict, names = "unique", null.ok = TRUE)
  if (length(par) == 0)
    return(integer(0L))

  length = par$len
  if (is.expression(length))
    length = eval(length, envir = dict)
  # setNames(as.integer(length), par$id)
  as.integer(length)
}

#' @export
getParamLengths.ParamSet = function(par, dict = NULL) {
  assertClass(par, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  if (isEmpty(par))
    return(integer(0L))

  lengths = extractSubList(par$pars, "len", simplify = FALSE)
  j = vlapply(par$pars, function(x) is.expression(x$len))
  lengths[j] = lapply(lengths[j], eval, envir = dict)
  setNames(as.integer(lengths), names(lengths))
}
