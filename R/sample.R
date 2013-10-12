#' Sample a random value from a parameter or a parameter set uniformly.
#'
#' Dependent parameters whose requirements are not satisfied are represented by a scalar NA in the output.
#' 
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @param discrete.names [\code{logical(1)}]\cr
#'   Should names be sampled for discrete / logical paramaters or values instead?
#'   Default is code {FALSE}.
#' @return The return type is determined by the type of the parameter. For a set a named list
#'   of such values in the correct order is returned.
#' @export
#' @examples 
#' # bounds are necessary here, can't sample with Inf bounds:
#' u <- makeNumericParam("x", lower=0, upper=1)
#' # returns a random number between 0 and 1:
#' sampleValue(u) 
#' 
#' p <- makeDiscreteParam("x", values=c("a","b","c"))
#' # can be either "a", "b" or "c"
#' sampleValue(p)
#' 
#' p <- makeIntegerVectorParam("x", len=2, lower=1, upper=5)
#' # vector of two random integers between 1 and 5:
#' sampleValue(p) 
#' 
#' ps <- makeParamSet(
#'   makeNumericParam("x", lower=1, upper=10),
#'   makeIntegerParam("y", lower=1, upper=10),
#'   makeDiscreteParam("z", values=1:2)
#' )
#' sampleValue(ps)
sampleValue = function(par, discrete.names=FALSE) {
  UseMethod("sampleValue")
}

#' @S3method sampleValue Param
sampleValue.Param = function(par, discrete.names=FALSE) {
  type = par$type
  if (par$type %in% c("numeric", "numericvector", "integer", "integervector"))
    if (any(is.infinite(c(par$lower, par$upper))))
      stop("Cannot sample with Inf bounds!")
  if (!is.null(par$len) && is.na(par$len))
    stop("Cannot sample with NA length!")
  if (type == "numeric") {
    runif(1, min=par$lower, max=par$upper)
  } else if (type == "numericvector") {
    runif(par$len, min=par$lower, max=par$upper)
  } else if (type == "integer") {
    as.integer(round(runif(1, min=par$lower-0.5, max=par$upper+0.5)))
  } else if (type == "integervector") {
    as.integer(round(runif(par$len, min=par$lower-0.5, max=par$upper+0.5)))
  } else if (type %in% c("discrete", "discretevector", "logical", "logicalvector")) {
    x = sample(names(par$values), par$len, replace=TRUE) 
    if (!discrete.names) {
      x = if (type  == "discretevector")
        par$values[x]
      else if (type  == "logicalvector")
        as.logical(x)
      else
        par$values[[x]]
    }
    return(x)
  } else if (type == "function") {
    stop("Cannot generate random value for function variable!")
  } else if (type == "untyped") {
    stop("Cannot generate random value for untyped variable!")
  }
}

#' @S3method sampleValue ParamSet
sampleValue.ParamSet = function(par, discrete.names=FALSE) {
  val = lapply(par$pars, sampleValue, discrete.names=discrete.names)
  setNames(lapply(seq_along(val), function(i) {
    if (!is.null(par$pars[[i]]$requires) && !requiresOk(par, val, i)) {
      type = par$pars[[i]]$type
      type = switch(type,
        numericvector = "numeric",
        integervector = "integer",
        logicalvector = "logical",
        type
      )
      as(NA, type)
     } else {
      val[[i]]
     }
  }), names(par$pars))
}


#' Sample n random values from a parameter or a parameter set uniformly.
#' 
#' Dependent parameters whose requirements are not satisfied are represented by a scalar NA in the output.
#'
#' @param par [\code{\link{Param}} | \code{\link{ParamSet}}]\cr
#'   Parameter or parameter set.
#' @param n [\code{integer(1)}]\cr
#'   Number of values.
#' @param discrete.names [\code{logical(1)}]\cr
#'   Should names be sampled for discrete / logical paramaters or values instead?
#'   Default is code {FALSE}.
#' @return [\code{list}]. For consistency always a list is returned.
#' @export
#' @examples 
#' p <- makeIntegerParam("x", lower=-10, upper=10)
#' sampleValues(p, 4)
#' 
#' p <- makeNumericParam("x", lower=-10, upper=10)
#' sampleValues(p, 4)
#' 
#' p <- makeLogicalParam("x")
#' sampleValues(p, 4)
#' 
#' ps <- makeParamSet(
#'   makeNumericParam("u", lower=1, upper=10),
#'   makeIntegerParam("v", lower=1, upper=10),
#'   makeDiscreteParam("w", values=1:2)
#' )
#' sampleValues(ps, 2)
sampleValues = function(par, n, discrete.names=FALSE) {
  checkArg(par, c("Param", "ParamSet"))
  n = convertInteger(n)
  checkArg(n, "integer", 1, na.ok=FALSE)
  replicate(n, sampleValue(par, discrete.names = discrete.names), simplify=FALSE)
}

