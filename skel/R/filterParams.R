#FIXME why does knitr print the returned S3 class twice in the example?
#' Get parameter subset of only certain parameters.
#' 
#' Parameter order is not changed.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param type [\code{character}]\cr
#'   Vector of allowed types, subset of: \dQuote{numeric}, \dQuote{integer}, \dQuote{numericvector}, \dQuote{integervector}, \dQuote{discrete}, \dQuote{discretevector}, \dQuote{logical}, \dQuote{logicalvector}, \dQuote{function}, \dQuote{untyped}.
#' @return [\code{\link{ParamSet}}].
#' @examples
#' ps <- makeParamSet(
#'   makeNumericParam("u", lower=1),
#'   makeIntegerParam("v", lower=1, upper=2),
#'   makeDiscreteParam("w", values=1:2),
#'   makeLogicalParam("x"),
#'   makeNumericParam("y")
#' )
#' # filter for numeric parameters
#' filterParams(ps, "numeric")
#' # filter for numeric and integer parameters
#' filterParams(ps, c("integer","numeric"))
#' @export 
filterParams = function(par.set, type) {
  checkArg(type, subset=c("numeric", "integer", "numericvector", "integervector", "discrete", "discretevector", "logical", "logicalvector", "function", "untyped"))
  par.set$pars = Filter(function(p) p$type %in% type, par.set$pars) 
  return(par.set)
}