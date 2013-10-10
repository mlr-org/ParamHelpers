#' Get lower / upper bounds and allowed discrete values for parameters.
#' 
#' \code{getLower} and \code{getUpper} return a numerical vector of lower and upper
#' bounds, \code{getValues} returns a list of possible value sets for discrete parameters.
#'
#' Parameters for which such bound make no sense - due to their type - are not present in the result. 
#' 
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to names of vector params?
#'   Default is \code{FALSE}.
#' @return [\code{vector} | \code{list}]. Named by parameter ids.
#' @export
#' @examples 
#' ps <- makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeNumericVectorParam("x", len=2, lower=c(0, 10), upper=c(1, 11))
#' )
#' getLower(ps)
#' getUpper(ps)
#'
#' ps <- makeParamSet(
#'   makeNumericParam("u"),
#'   makeDiscreteParam("v", values=c("a", "b")),
#'   makeDiscreteParam("w", values=list(a=list(), b=NULL))
#' )
#' getValues(ps)
getLower = function(par.set, with.nr=FALSE) {
  checkArg(par.set, "ParamSet")
  par.set = filterParams(par.set, c("numeric", "integer", "numericvector", "integervector")) 
  if (length(par.set$pars) == 0)
    return(numeric(0))
  bounds = lapply(par.set$pars, function(p) p$lower)
  bounds = do.call(c, bounds)
  names(bounds) = getParamIds(par.set, repeated=TRUE, with.nr=with.nr)
  return(bounds)
}

#' @export
#' @rdname getLower 
getUpper = function(par.set, with.nr=FALSE) {
  checkArg(par.set, "ParamSet")
  par.set = filterParams(par.set, c("numeric", "integer", "numericvector", "integervector")) 
  if (length(par.set$pars) == 0)
    return(numeric(0))
  bounds = lapply(par.set$pars, function(p) p$upper)
  bounds = do.call(c, bounds)
  if (length(bounds) > 0)
    names(bounds) = getParamIds(par.set, repeated=TRUE, with.nr=with.nr)
  return(bounds)
}

#' @export
#' @rdname getLower 
getValues = function(par.set) {
  checkArg(par.set, "ParamSet")
  par.set = filterParams(par.set, c("discrete", "discretevector", "logical", "logicalvector"))
  if (length(par.set$pars) == 0)
    return(list())
  lapply(par.set$pars, function(p) p$values)
}

