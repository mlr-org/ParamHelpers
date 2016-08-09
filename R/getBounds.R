#' @title Get lower / upper bounds and allowed discrete values for parameters.
#'
#' @description
#' \code{getLower} and \code{getUpper} return a numerical vector of lower and upper
#' bounds, \code{getValues} returns a list of possible value sets for discrete parameters.
#'
#' Parameters for which such bound make no sense - due to their type - are not present in the result.
#'
#' @template arg_parset
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to names of vector params?
#'   Default is \code{FALSE}.
#' @template arg_dict
#' @return [\code{vector} | \code{list}]. Named by parameter ids.
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeNumericVectorParam("x", len = 2, lower = c(0, 10), upper = c(1, 11))
#' )
#' getLower(ps)
#' getUpper(ps)
#'
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
#'   makeNumericParam("u"),,
#'   makeDiscreteParam("w", values = list(a = list(), b = NULL))
#' )
#' getValues(ps)
getLower = function(par.set, with.nr = FALSE, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  return(getBounds(par.set, type.of.bounds = "lower", with.nr = with.nr, dict = dict))
}

#' @export
#' @rdname getLower
getUpper = function(par.set, with.nr = FALSE, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  return(getBounds(par.set, type.of.bounds = "upper", with.nr = with.nr, dict = dict))
}

#' @export
#' @rdname getLower
getValues = function(par.set, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  assertClass(par.set, "ParamSet")
  types = getParamTypes(par.set)
  is.disc = types %in% c("discrete", "discretevector", "logical", "logicalvector")
  if (!any(is.disc))
    return(list())
  parset.has.expression.value = vlapply(par.set$pars[is.disc], function(p) {
    any(vlapply(p$values, is.expression))
  })
  if (any(parset.has.expression.value) && missing(dict))
    stop("You need to provide a task to get the values.")
  lapply(par.set$pars[is.disc], function(p) { eval(p$values, envir = dict)})
  lapply(par.set$pars[is.disc], function(p) p$values)
}

# common functionality of getLower and getUpper
getBounds = function(par.set, type.of.bounds, with.nr = FALSE, dict = NULL) {
  assertClass(par.set, "ParamSet")
  assertList(dict, names = "unique", null.ok = TRUE)
  # if we dont have numerics, return empty vector
  if (!hasNumeric(par.set, include.int = TRUE))
    return(numeric(0L))
  # filter to numerics, and get bounds, flat-join them and name them
  psnum = filterParamsNumeric(par.set,  include.int = TRUE)
  bounds = lapply(psnum$pars, function(p) p[[type.of.bounds]])

  # if any of the bounds is given with an expression, it needs to be evaluated first
  if (any(vlapply(bounds, is.expression))) {
    if (is.null(dict))
      stop("You need to provide a dictionary to get the bounds.")
    bounds = lapply(bounds, eval, envir = dict)
  }

  # FIXME: document this
  bounds = lapply(names(bounds), function(id) {
    len = par.set$pars[[id]]$len
    x = bounds[[id]]
    if (length(x) == 1L && len > 1L)
      return(rep(x, len))
    return(x)
  })

  bounds = do.call(c, bounds)
  names(bounds) = getParamIds(psnum, repeated = TRUE, with.nr = with.nr)
  return(bounds)
}
