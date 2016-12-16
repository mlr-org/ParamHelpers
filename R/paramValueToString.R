#' @title Convert a value to a string.
#'
#' @description
#' Useful helper for logging.
#' For discrete parameter values always the name of the discrete value is used.
#'
#' @template arg_par_or_set
#' @param x [any]\cr
#'   Value for parameter or value for parameter set. In the latter case it must be named list.
#'   For discrete parameters their values must be used, not their names.
#' @param show.missing.values [\code{logical(1)}]\cr
#'   Display \dQuote{NA} for parameters, which have no setting, because their requirements are not
#'   satisfied (dependent parameters), instead of displaying nothing?
#'   Default is \code{FALSE}.
#' @param num.format [\code{character(1)}]\cr
#'   Number format for output of numeric parameters. See the details section of the manual for
#'   \code{\link[base]{sprintf}} for details.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' p = makeNumericParam("x")
#' paramValueToString(p, 1)
#' paramValueToString(p, 1.2345)
#' paramValueToString(p, 0.000039)
#' paramValueToString(p, 8.13402, num.format = "%.2f")
#'
#' p = makeIntegerVectorParam("x", len=2)
#' paramValueToString(p, c(1L, 2L))
#'
#' p = makeLogicalParam("x")
#' paramValueToString(p, TRUE)
#'
#' p = makeDiscreteParam("x", values=list(a=NULL, b=2))
#' paramValueToString(p, NULL)
#'
#' ps = makeParamSet(
#'   makeNumericVectorParam("x", len=2L),
#'   makeDiscreteParam("y", values=list(a=NULL, b=2))
#' )
#' paramValueToString(ps, list(x=c(1,2), y=NULL))
paramValueToString = function(par, x, show.missing.values = FALSE, num.format = "%.3g") {
  assertFlag(show.missing.values)
  assertString(num.format)
  UseMethod("paramValueToString")
}

#' @export
paramValueToString.Param = function(par, x, show.missing.values = FALSE, num.format = "%.3g") {
  # handle missings
  if (isScalarNA(x)) {
    if (show.missing.values)
      return("NA")
    else
      return("")
  }
  if (isDiscrete(par, include.logical = FALSE))
    x = discreteValueToName(par, x)
  s = valueToString(x, num.format = num.format)
}

#' @export
paramValueToString.ParamSet = function(par, x, show.missing.values = FALSE, num.format = "%.3g") {
  assertList(x)
  if (!isProperlyNamed(x))
    stop("'x' must be a properly named list!")
  rest = setdiff(names(x), names(par$pars))
  if (length(rest) > 0L)
    stopf("Not all names of 'x' occur in par set 'par': %s", collapse(rest))
  res = character(0L)
  for (i in seq_along(x)) {
    pn = names(x)[i]
    val = x[[pn]]
    if (show.missing.values || !isScalarNA(val))  {
      p = par$pars[[pn]]
      res[length(res)+1] = sprintf("%s=%s", pn, paramValueToString(p, val, show.missing.values, num.format))
    }
  }
  return(collapse(res, sep = "; "))
}


valueToString = function(x, num.format = "%.3g") {
  cl = class(x)[1L]

  if (cl == "numeric")
    paste(sprintf(num.format, x), collapse=",")
  else if (cl == "integer")
    paste(as.character(x), collapse=",")
  else if (cl == "logical")
    paste(as.character(x), collapse=",")
  else if (cl == "character")
    collapse(x)
  else if (cl == "function")
    "<function>"
  else if (cl == "expression")
    as.character(x)
  else
    sprintf("<%s>", cl)
}


