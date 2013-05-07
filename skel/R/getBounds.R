#' Get lower bounds for numerical / integer parameters.
#' 
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return Numeric vector of lower bounds. The lower
#'   bounds are all concatenated and the vector is named by the parameter names with 
#'   \code{getParamIds(..., repeated=TRUE, with.nr=FALSE)}.
#' @export
#' @examples 
#' ps <- makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeNumericVectorParam("x", len=2, lower=c(0, 10), upper=c(1, 11))
#' )
#' getLower(ps)
getLower = function(par.set) {
  checkArg(par.set, "ParamSet")
  par.set = filterParams(par.set, c("numeric", "integer", "numericvector", "integervector")) 
  if (length(par.set$pars) == 0)
    return(numeric(0))
  bounds = lapply(par.set$pars, function(p) p$lower)
  bounds = do.call(c, bounds)
  names(bounds) = getParamIds(par.set, repeated=TRUE, with.nr=FALSE)
  return(bounds)
}

#' Get upper bounds for numerical / integer parameters.
#' 
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return Numeric vector of upper bounds. The upper
#'   bounds are all concatenated and the vector is named by the parameter names with 
#'   \code{getParamIds(..., repeated=TRUE, with.nr=FALSE)}.
#' @export
#' @examples 
#' ps <- makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeNumericVectorParam("x", len=2, lower=c(0, 10), upper=c(1, 11))
#' )
#' getUpper(ps)
getUpper = function(par.set) {
  checkArg(par.set, "ParamSet")
  par.set = filterParams(par.set, c("numeric", "integer", "numericvector", "integervector")) 
  if (length(par.set$pars) == 0)
    return(numeric(0))
  bounds = lapply(par.set$pars, function(p) p$upper)
  bounds = do.call(c, bounds)
  if (length(bounds) > 0)
    names(bounds) = getParamIds(par.set, repeated=TRUE, with.nr=FALSE)
  return(bounds)
}

#' Get possible values for discrete parameters.
#' 
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @return [\code{list}]. A named list of named lists of values. 
#' @export
#' @examples 
#' ps <- makeParamSet(
#'   makeNumericParam("u"),
#'   makeDiscreteParam("v", values=c("a", "b")),
#'   makeDiscreteParam("w", values=list(a=list(), b=NULL))
#' )
#' getValues(ps)
getValues = function(par.set) {
  checkArg(par.set, "ParamSet")
  par.set = filterParams(par.set, c("discrete", "discretevector", "logical", "logicalvector"))
  if (length(par.set$pars) == 0)
    return(list())
  lapply(par.set$pars, function(p) p$values)
}

