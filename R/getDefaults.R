#' @title Return defaults of parameters in parameter set.
#'
#' @description
#' Return defaults of single parameters or parameters in a parameter set or a list of parameters.
#'
#' @param obj ([Param()] | [ParamSet()] | `list`)\cr
#'   Parameter, parameter set or list of parameters, whose defaults should be
#'   extracted. In case the default values contain expressions, they will be
#'   evaluated using the provided dictionary (`dict`).
#' @param include.null (`logical(1)`)\cr
#'   Include `NULL` entries for parameters without default values in the result
#'   list? Note that this can be slightly dangerous as `NULL` might be used as
#'   default value for other parameters. Default is `FALSE`.
#' @template arg_dict
#' @return named `list`. Named (and in case of a [ParamSet()], in the same order).
#'   Parameters without defaults are not present in the list.
#' @examples
#' ps1 = makeParamSet(
#'   makeDiscreteParam("x", values = c("a", "b"), default = "a"),
#'   makeNumericVectorParam("y", len = 2),
#'   makeIntegerParam("z", default = 99)
#' )
#' getDefaults(ps1, include.null = TRUE)
#'
#' ps2 = makeParamSet(
#'   makeNumericVectorParam("a", len = expression(k), default = expression(p)),
#'   makeIntegerParam("b", default = 99),
#'   makeLogicalParam("c")
#' )
#' getDefaults(ps2, dict = list(k = 3, p = 5.4))
#' @export
getDefaults = function(obj, include.null = FALSE, dict = NULL) {
  UseMethod("getDefaults")
}

#' @export
getDefaults.Param = function(obj, include.null = FALSE, dict = NULL) {

  assertClass(obj, "Param")
  assertFlag(include.null)
  assertList(dict, names = "unique", null.ok = TRUE)

  # no param = no default
  if (length(obj) == 0L) {
    return(NULL)
  }

  def = obj$default
  if (is.null(def) || !obj$has.default) {
    return(NULL)
  }
  if (is.expression(def)) {
    def = eval(def, envir = dict)
  }

  # evaluate length in case it is defined with an expression
  if (is.expression(obj$len)) {
    obj$len = getParamLengths(par = obj, dict = dict)
  }

  # replicate default according to length of param
  if ((length(def) == 1L) && !is.na(obj$len) && obj$len > 1L) {
    def = rep(def, obj$len)
  }

  return(def)
}

#' @export
getDefaults.ParamSet = function(obj, include.null = FALSE, dict = NULL) {

  assertClass(obj, "ParamSet")
  assertFlag(include.null)
  assertList(dict, names = "unique", null.ok = TRUE)

  # if the ParamSet is empty, there are no defaults
  if (isEmpty(obj)) {
    return(list())
  }

  # extract list with defaults of all params
  defs = extractSubList(obj$pars, "default", simplify = FALSE)
  if (!include.null) {
    # if all defaults are NULL (and NULLs are not allowed) return empty list
    if (all(vlapply(defs, is.null))) {
      return(list())
    }
    j = vlapply(obj$pars, function(x) x$has.default)
    if (!any(j)) {
      return(list())
    }
    # extract ids of params with non-NULL defaults
    ids = names(defs)[j]
  } else {
    # consider all params
    ids = names(defs)
  }

  # extract defaults of all considerable params
  setNames(lapply(obj$pars[ids], getDefaults, include.null = include.null, dict = dict), ids)
}

#' @export
getDefaults.list = function(obj, include.null = FALSE, dict = NULL) {

  assertClass(obj, "list")
  assertFlag(include.null)
  assertList(dict, names = "unique", null.ok = TRUE)

  # if the list is empty, there are no defaults
  if (length(obj) == 0L) {
    return(list())
  }

  # extract list with defaults of all params
  defs = extractSubList(obj, "default", simplify = FALSE)
  if (!include.null) {
    # if all defaults are NULL (and NULLs are not allowed) return empty list
    if (all(vlapply(defs, is.null))) {
      return(list())
    }
    j = vlapply(obj, function(x) x$has.default)
    if (!any(j)) {
      return(list())
    }
    # extract ids of params with non-NULL defaults
    ids = names(defs)[j]
  } else {
    # consider all params
    ids = names(defs)
  }

  # extract defaults of all considerable params
  setNames(lapply(obj[j], getDefaults, include.null = include.null, dict = dict), ids)
}
