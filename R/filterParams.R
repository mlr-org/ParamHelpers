#' Get parameter subset of only certain parameters.
#'
#' Parameter order is not changed.
#'
#' @template arg_parset
#' @param type [\code{character}]\cr
#'   Vector of allowed types, subset of: \dQuote{numeric}, \dQuote{integer}, \dQuote{numericvector},
#'   \dQuote{integervector}, \dQuote{discrete}, \dQuote{discretevector}, \dQuote{logical},
#'   \dQuote{logicalvector}, \dQuote{function}, \dQuote{untyped}.
#' @return [\code{\link{ParamSet}}].
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u", lower = 1),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeLogicalParam("x"),
#'   makeNumericParam("y")
#' )
#' # filter for numeric parameters
#' filterParams(ps, "numeric")
#' # filter for numeric and integer parameters
#' filterParams(ps, c("integer","numeric"))
#' @export
filterParams = function(par.set, type) {
  assertSubset(type, c("numeric", "integer", "numericvector", "integervector", "discrete",
    "discretevector", "logical", "logicalvector", "function", "untyped"))
  if (!is.null(par.set$forbidden))
    stopf("Operation not allowed for param set with forbidden region currently!")
  par.set$pars = Filter(function(p) p$type %in% type, par.set$pars)
  return(par.set)
}
