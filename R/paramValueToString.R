#' Convert a value to a string.
#'
#' Useful helper for logging.
#' For discrete parameter values always the name of the discrete value is used.
#' 
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @param x [any]\cr
#'   Value for parameter or value for parameter set. In the latter case it must be named list.
#' @param show.missing.values [\code{logical(1)}]\cr
#'   Display \dQuote{NA} for parameters, which have no setting, because their requirements are not satified 
#'   (dependent parameters), instead of nothing?
#'   Default is \code{FALSE}.
#' @return [\code{character(1)}].
#' @export
#' @examples
#' p <- makeNumericParam("x")
#' paramValueToString(p, 1) # "1.00"
#' paramValueToString(p, 1.2345) # "1.23"
#'
#' p <- makeIntegerVectorParam("x", len=2)
#' paramValueToString(p, c(1L, 2L)) # 1,2
#'
#' p <- makeLogicalParam("x")
#' paramValueToString(p, TRUE) # "TRUE"
#'
#' p <- makeDiscreteParam("x", values=list(a=NULL, b=2))
#' paramValueToString(p, NULL) # "a"
paramValueToString = function(par, x, show.missing.values=FALSE) {
  UseMethod("paramValueToString")
}

#' @S3method paramValueToString Param
paramValueToString.Param = function(par, x, show.missing.values=FALSE) {
  # handle missings
  if (isMissingValue(x)) {
    if (show.missing.values)
      return("NA")
    else
      return("")
  }

  type = par$type
  if (type == "numeric")
    sprintf("%.2f", x)
  else if (type == "numericvector")
    paste(sprintf("%.2f", x), collapse=",")
  else if (type == "integer")
    as.character(x)
  else if (type == "integervector")
    paste(as.character(x), collapse=",")
  else if (type == "logical")
    as.character(x)
  else if (type == "logicalvector")
    paste(as.character(x), collapse=",")
  else if (type == "discrete")
    discreteValueToName(par, x)
  else if (type == "discretevector")
    collapse(discreteValueToName(par, x))
  else if (type == "function")
    "<function>"
  else if (type == "untyped")
    sprintf("<%s>", class(x)[1])
}

#' @S3method paramValueToString ParamSet
paramValueToString.ParamSet = function(par, x, show.missing.values=FALSE) {
  res = character(0)
  for (i in seq_along(x)) {
    pn = names(x)[i]
    val = x[[pn]]
    if (show.missing.values || !isMissingValue(val))  {             
      p = par$pars[[pn]]
      res[length(res)+1] = sprintf("%s=%s", pn, paramValueToString(p, val, show.missing.values))          
    }
  }
  return(collapse(res, sep=","))
}
