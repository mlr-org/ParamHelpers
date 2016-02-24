#' Get lower / upper bounds and allowed discrete values for parameters.
#'
#' \code{getLower} and \code{getUpper} return a numerical vector of lower and upper
#' bounds, \code{getValues} returns a list of possible value sets for discrete parameters.
#'
#' Parameters for which such bound make no sense - due to their type - are not present in the result.
#'
#' @template arg_parset
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to names of vector params?
#'   Default is \code{FALSE}.
#' @param envir [\code{list} | \code{NULL}]\cr
#'   If a bound is given by an expression, this list will be used as a
#'   dictionary, in which the unknown arguments of the expression are defined.
#'   The default is \code{NULL}.
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
#'   makeIntegerParam("v", lower = expression(ceiling(p / 3)), upper = 2, envir = list(p = NULL)),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeNumericVectorParam("x", len = 2, lower = c(0, 10), upper = c(1, 11))
#' )
#' getLower(ps, envir = list(p = 7))
#' getUpper(ps)
#'
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeDiscreteParam("v", values = c("a", "b")),
#'   makeDiscreteParam("w", values = list(a = list(), b = NULL))
#' )
#' getValues(ps)
#' @export
getLower = function(par.set, with.nr = FALSE, envir = NULL) {
  return(getBounds(par.set, type.of.bounds = "lower", with.nr = with.nr, envir = envir))
}

#' @export
#' @rdname getLower
getUpper = function(par.set, with.nr = FALSE, envir = NULL) {
  return(getBounds(par.set, type.of.bounds = "upper", with.nr = with.nr, envir = envir))
}

#' @export
#' @rdname getLower
getValues = function(par.set, envir = NULL) {
  types = getParamTypes(par.set)
  is.disc = types %in% c("discrete", "discretevector", "logical", "logicalvector")
  if (!any(is.disc))
    return(list())
  parset.has.expression.value = vapply(par.set$pars[is.disc], function(p) {
    value.has.expression = vapply(p$values, is.expression, logical(1L))
    any(value.has.expression)
  }, logical(1L))
  if (any(parset.has.expression.value) & missing(envir))
    stop("You need to provide a task to get the values.")
  lapply(par.set$pars[is.disc], function(p) {
    eval(p$values, envir = envir)})
}

# common functionality of getLower and getUpper
getBounds = function(par.set, type.of.bounds, with.nr = FALSE, envir = NULL) {
  assertClass(par.set, "ParamSet")
  types = getParamTypes(par.set)
  is.num = types %in% c("numeric", "integer", "numericvector", "integervector")
  if (!any(is.num))
    return(numeric(0))
  bounds = lapply(par.set$pars[is.num], function(p) p[[type.of.bounds]])
  # if any of the bounds is given with an expression, it needs to be evaluated first
  if (any(vapply(bounds, is.expression, logical(1L)))) {
    if (is.null(envir))
      stop("You need to provide an environment to get the bounds.")
    bounds = lapply(bounds, eval, envir = envir)
  }
  bounds = do.call(c, bounds)
  names(bounds) = getParamIds2(par.set$pars[is.num], repeated = TRUE, with.nr = with.nr)
  return(bounds)
}
