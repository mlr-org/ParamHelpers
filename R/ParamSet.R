#' Construct a parameter set.
#' 
#' \code{makeParamSet}: Contruct from a bunch of parameters.
#' 
#' Multiple sets can be concatenated with \code{c}.
#' 
#' The constructed S3 class is simply a list that contains the element \code{pars}.
#' \code{pars} is a list of the passed parameters, named by their ids. 
#'
#' @param ... [\code{\link{Param}}]\cr
#'   Parameters.
#' @param params [list of \code{\link{Param}}]\cr
#'   List of parameters, alternative way instead of using \code{...}.
#' @return [\code{\link{ParamSet}}].
#' @aliases ParamSet
#' @export 
#' @examples 
#' makeParamSet(
#'   makeNumericParam("u", lower=1),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeLogicalParam("x"),
#'   makeDiscreteVectorParam("y", len=2, values=c("a", "b"))
#' )
makeParamSet = function(..., params) {
  pars = list(...)
  if (length(pars) > 0 && !missing(params))
    stop("You can only use one of ... or params!")
  if (!missing(params)) {
    checkListElementClass(params, "Param")
    pars = params
  } else {
    checkListElementClass(pars, "Param")
  }
  ns = extractSubList(pars, "id")
  if (any(duplicated(ns)))
    stop("All parameters must have unique names!")
  names(pars) = ns
  x = list(pars=pars)
  class(x) = "ParamSet"
  return(x)
}

#' @S3method print ParamSet
print.ParamSet = function(x, ...) {
  if (length(x$pars) == 0)
    print("Empty parameter set.")
  else  
    sapply(x$pars, print)
}

#' @S3method c ParamSet
c.ParamSet = function(..., recursive=FALSE) {
  pss = list(...)
  pars = Reduce(c, lapply(pss, function(ps) ps$pars))
  do.call(makeParamSet, pars)
}

#' \code{makeNumericParamSet}: Convenience function for numerics.
#'
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param len [\code{integer(1)}]\cr
#'   Length of vector.
#' @param lower [\code{numeric}]\cr
#'   Lower bound. 
#'   Default is \code{-Inf}.
#' @param upper [\code{numeric}] \cr
#'   Upper bound. 
#'   Default is \code{Inf}.
#' @param vector [\code{logical(1)}] \cr
#'   Should a \code{NumericVectorParam} be used instead of 
#'   n \code{NumericParam} objects?
#'   Default is \code{TRUE}.
#' @rdname makeParamSet
#' @export 
makeNumericParamSet = function(id="x", len, lower=-Inf, upper=Inf, vector=TRUE) {
	checkArg(id, "character", len=1L, na.ok=FALSE)
  if (missing(len)) {
    if (!missing(lower))
      len = length(lower)
    else if (!missing(upper))
      len = length(upper)
  } else {
		len = convertInteger(len)
		checkArg(len, "integer", len=1L, na.ok=FALSE)
	}
  if (is.numeric(lower) && length(lower) == 1)
    lower = rep(lower, len)
  if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, len)
	checkArg(lower, "numeric", len=len, na.ok=FALSE)
	checkArg(upper, "numeric", len=len, na.ok=FALSE)
	checkArg(vector, "logical", len=1L, na.ok=FALSE)
  if (vector) {
    makeParamSet(makeNumericVectorParam(id=id, len=len, lower=lower, upper=upper))
  } else {
    makeParamSet(params=lapply(1:len, function(i)
      makeNumericParam(id=paste(id, i, sep=""), lower=lower[i], upper=upper[i])))
  } 
}