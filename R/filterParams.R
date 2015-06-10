#' Get parameter subset of only certain parameters.
#'
#' Parameter order is not changed.
#'
#' @template arg_parset
#' @param type [\code{NULL} | \code{character}]\cr
#'   Vector of allowed types, subset of: \dQuote{numeric}, \dQuote{integer}, \dQuote{numericvector},
#'   \dQuote{integervector}, \dQuote{discrete}, \dQuote{discretevector}, \dQuote{logical},
#'   \dQuote{logicalvector}, \dQuote{function}, \dQuote{untyped}.
#'   Setting \code{type = NULL} allows the consideration of all types.
#' @param tunable [\code{logical}]\cr
#'   Vector of allowed values for the property \code{tunable}. Accepted arguments are
#'   \code{TRUE}, \code{FALSE} or \code{c(TRUE, FALSE)}.
#'   The default is \code{c(TRUE, FALSE)}, i.e. none of the parameters will be removed.
#' @return [\code{\link{ParamSet}}].
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u", lower = 1),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeLogicalParam("x"),
#'   makeNumericParam("y", tunable = FALSE)
#' )
#' # filter for numeric parameters
#' filterParams(ps, "numeric")
#' # filter for numeric and integer parameters
#' filterParams(ps, c("integer","numeric"))
#' # filter for tunable, numeric parameters
#' filterParams(ps, "numeric", TRUE)
#' # filter for all tunable parameters
#' filterParams(ps, NULL, TRUE)
#' @export
filterParams = function(par.set, type, tunable = c(TRUE, FALSE)) {
  assertSubset(type, c("numeric", "integer", "numericvector", "integervector", "discrete",
    "discretevector", "logical", "logicalvector", "function", "untyped"))
  assertLogical(tunable, min.len = 0L, max.len = 2L, unique = TRUE)
  if (!is.null(par.set$forbidden))
    stopf("Operation not allowed for param set with forbidden region currently!")
  if (!is.null(type))
    par.set$pars = Filter(function(p) p$type %in% type, par.set$pars)
  par.set$pars = Filter(function(p) p$tunable %in% tunable, par.set$pars)
  return(par.set)
}
