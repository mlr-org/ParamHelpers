#' @title Get lower / upper bounds and allowed discrete values for parameters.
#'
#' @description
#' \code{getLower} and \code{getUpper} return a numerical vector of lower and upper
#' bounds, \code{getValues} returns a list of possible value sets for discrete parameters.
#'
#' Parameters for which such bound make no sense - due to their type - are not present in the result.
#'
#' @param obj [\code{\link{Param}} | \code{\link{ParamSet}} | \code{list}]\cr
#'   Parameter, parameter set or list of parameters, whose boundaries and/or
#'   values should be extracted. In case the boundaries or values contain expressions,
#'   they will be evaluated using the provided dictionary \code{dict}.
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to names of vector params?
#'   Default is \code{FALSE}.
#' @template arg_dict
#' @return [\code{vector} | \code{list}]. Named by parameter ids.
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeDiscreteParam("v", values = c("a", "b")),
#'   makeIntegerParam("w", lower = expression(ceiling(p / 3)), upper = 2),
#'   makeDiscreteParam("x", values = 1:2),
#'   makeNumericVectorParam("y", len = 2, lower = c(0, 10), upper = c(1, 11)),
#'   keys = "p"
#' )
#' getLower(ps, dict = list(p = 7))
#' getUpper(ps)
#'
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeDiscreteParam("w", values = list(a = list(), b = NULL))
#' )
#' getValues(ps)
#'
#' par.vals = list(
#'   u = makeNumericParam("u"),
#'   v = makeIntegerParam("v", lower = 1, upper = 2),
#'   w = makeDiscreteParam("w", values = 1:2),
#'   x = makeNumericVectorParam("x", len = 2, lower = c(3, 1), upper = expression(n))
#' )
#' getLower(par.vals)
#' getUpper(par.vals, dict = list(n = 12))
#' @export
getLower = function(obj, with.nr = FALSE, dict = NULL) {
  getBounds(obj, type.of.bounds = "lower", with.nr = with.nr, dict = dict)
}

#' @export
#' @rdname getLower
getUpper = function(obj, with.nr = FALSE, dict = NULL) {
  getBounds(obj, type.of.bounds = "upper", with.nr = with.nr, dict = dict)
}

#' @export
#' @rdname getLower
getValues = function(obj, dict = NULL) {
  UseMethod("getValues")
}

#' @export
getValues.Param = function(obj, dict = NULL) {
  assertClass(obj, "Param")
  assertList(dict, names = "unique", null.ok = TRUE)
  # values are only possible for params of the types above
  if (!isDiscrete(obj))
    return(NULL)
  # error if dict is not defined, but values contains expression
  if (is.null(dict) && hasExpression(obj))
    stop("You need to provide a dictionary to get the values.")
  eval(obj$values, envir = dict)
}

#' @export
getValues.ParamSet = function(obj, dict = NULL) {
  assertClass(obj, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  types = getParamTypes(obj)
  is.disc = types %in% getTypeStringsDiscrete()
  # only consider params with one of the types from above
  if (!any(is.disc))
    return(list())
  # error if dict is not defined, but at least one value contains an expression
  if (missing(dict) && any(vlapply(obj$pars[is.disc], hasExpression)))
    stop("You need to provide a dictionary to get the values.")
  lapply(obj$pars[is.disc], function(p) eval(p$values, envir = dict))
}

#' @export
getValues.list = function(obj, dict = NULL) {
  assertClass(obj, "list")
  assertList(dict, names = "unique", null.ok = TRUE)

  # create a pseudo-ParamSet and use of getValues.ParamSet
  par.set = list(pars = obj)
  class(par.set) = "ParamSet"
  values = getValues(obj = par.set, dict = dict)
  return(values)
}


getBounds = function(obj, type.of.bounds, with.nr = FALSE, dict = NULL) {
  UseMethod("getBounds")
}

# workhorse for getLower and getUpper (for Param)
getBounds.Param = function(obj, type.of.bounds, with.nr = FALSE, dict = NULL) {
  assertClass(obj, "Param")
  assertList(dict, names = "unique", null.ok = TRUE)
  # if the Param is non-numeric, return NULL
  if (!(obj$type %in% getTypeStringsNumeric()))
    return(NULL)
  # filter to numerics, and get bounds, flat-join them and name them
  bound = obj[[type.of.bounds]]

  # if the bound is an expression, it needs to be evaluated first
  if (is.expression(bound)) {
    if (is.null(dict))
      stop("You need to provide a dictionary to get the bounds.")
    bound = eval(bound, envir = dict)
  }

  # assure that the length of the bound corresponds to the pre-defined length
  len = obj$len
  if (length(bound) == 1L && !is.na(len) && len > 1L)
    bound = rep(bound, len)

  setNames(bound, getParamIds(obj, repeated = TRUE, with.nr = with.nr))
}

# workhorse for getLower and getUpper for ParamSet
getBounds.ParamSet = function(obj, type.of.bounds, with.nr = FALSE, dict = NULL) {
  assertClass(obj, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  # if we don't have numerics, return empty vector
  if (!hasNumeric(obj, include.int = TRUE))
    return(numeric(0L))
  # filter to numerics
  psnum = filterParamsNumeric(obj,  include.int = TRUE)

  # get bounds of all numeric Params, flat-join and name them
  bounds = lapply(psnum$pars, function(p)
    getBounds(obj = p, type.of.bounds = type.of.bounds, with.nr = with.nr, dict = dict))

  setNames(unlist(bounds), getParamIds(psnum, repeated = TRUE, with.nr = with.nr))
}

# workhorse for of getLower and getUpper for regular lists
getBounds.list = function(obj, type.of.bounds, with.nr = FALSE, dict = NULL) {
  assertClass(obj, "list")
  assertList(dict, names = "unique", null.ok = TRUE)

  # create a pseudo-ParamSet and make use of the existing functions
  par.set = list(pars = obj)
  class(par.set) = "ParamSet"

  bounds = getBounds(obj = par.set, type.of.bounds = type.of.bounds, with.nr = with.nr, dict = dict)
  return(bounds)
}
