#' Discretizes a given parameter to a sequence of values.
#'
#' @param par [\code{\link{Param}}]
#'   The parameter you want to discretize.
#' @param lower [\code{numeric}(1)]
#'   Lower bound.
#'   Will overwrite the value in Param if set.
#' @param upper [\code{numeric}(1)]
#'   Upper bound.
#'   Will overwrite the value in Param if set.
#' @param by [\code{numeric}(1)]
#'   Increment of the sequence.
#'   Do not pass this if you use \code{length.out}.
#' @param length.out [\code{numeric}(1)]
#'   Desired length of the sequence.
#'   Do not pass this if you use \code{by}.
#' @param trafo [\code{logical(1)}]
#'   Should the transformation be applied?
#' @return [\code{\link{Param}}].
#'   A \code{Param} of the type \code{discrete}.
#' @examples
#' par <- makeNumericParam("x1", lower=-2, upper=1)
#' disc.par <- discretizeParam(par, by=0.25)
#' @export

discretizeParam = function(par, lower, upper, by, length.out, trafo = FALSE) {
  checkArg(par, "Param")
  if(!missing(lower))
    checkArg(lower, "numeric", na.ok = FALSE, len = 1)
  if(!missing(upper))
    checkArg(upper, "numeric", na.ok = FALSE, len = 1)
  if(!missing(by))
    checkArg(by, "numeric", na.ok = FALSE, len = 1)
  if(!missing(length.out)){
    length.out = convertInteger(length.out)
    checkArg(length.out, "integer", na.ok = FALSE, len = 1)
  }

  supported.discrete = c("discrete", "logical")
  supported.cont = c("integer", "numeric")

  if (par$type %in% supported.discrete) {
    par.disc = par
  } else if (par$type %in% supported.cont) {
    if(!missing(by) && !missing(length.out))
      stop("by and length.out cannot be combined!")
    if(missing(by) && missing(length.out))
      stop("by or length.out has to be given!")
    if (missing(lower))
      lower = par$lower
    if (missing(upper))
      upper = par$upper
    if (any(is.infinite(c(lower, upper))))
      stop("discretizeParam requires finite box constraints!")

    if(!missing(by)) {
      disc.vals = seq(from = lower, to = upper, by = by)
    } else if (!missing(length.out)) {
      disc.vals = seq(from = lower, to = upper, length.out = length.out)
    }
    if(trafo && !is.null(par$trafo)){
      disc.vals = par$trafo(disc.vals)
    }
    #FIXME: Is it desired behaviour to get just rounded values?
    if(par$type == "integer")
      disc.vals = as.integer(unique(round(disc.vals)))
    par.disc = makeDiscreteParam(id = par$id, values = disc.vals, trafo = par$trafo, requires = par$requires)
  } else {
    stop(sprintf("ParamType %s is not supported to be discretized.", par$type))
  }
  par.disc
}
