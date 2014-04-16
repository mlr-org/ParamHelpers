#' Discretizes a given parameter to a sequence of values.
#'
#' Discrete and logical parameters are directly returned, without change.
#' For integer parameters, the generated value sequence is rounded, then made unique.
#'
#' @param par [\code{\link{Param}}]
#'   The parameter you want to discretize.
#' @param lower [\code{numeric(1)}]
#'   Lower bound.
#'   Default is to take the bound of the parameter, if it exists.
#' @param upper [\code{numeric(1)}]
#'   Upper bound.
#'   Default is to take the bound of the parameter, if it exists.
#' @param by [\code{numeric(1)}]
#'   Increment of the sequence.
#'   Do not pass this if you use \code{length.out}.
#' @param length.out [\code{numeric(1)}]
#'   Desired length of the sequence.
#'   Do not pass this if you use \code{by}.
#' @param trafo [\code{logical(1)}]
#'   Should the transformation be applied to the resulting discrete values?
#' @return [\code{\link{Param}}].
#'   A \code{Param} of the type \code{discrete}.
#' @examples
#' par <- makeNumericParam("x1", lower = -2, upper = 1)
#' discretizeParam(par, by = 0.25)
#' @export
discretizeParam = function(par, lower, upper, by, length.out, trafo = FALSE) {
  checkArg(par, "Param")
  len = par$len

  if (par$type %in% c("untyped", "function"))
    stopf("Params of type %s cannot be discretized!", par$type)

  if (isNumeric(par)) {
    if (missing(lower)) {
      lower = par$lower
    } else {
      if (length(lower) == 1L)
        lower = rep(lower, len)
      checkArg(lower, "numeric", na.ok = FALSE, len = len)
      if (!isFeasible(par, lower))
        stop("'lower' must be a feasible value!")
    }
    if (missing(upper)) {
      upper = par$upper
    } else {
      if (length(upper) == 1L)
        upper = rep(upper, len)
      checkArg(upper, "numeric", na.ok = FALSE, len = len)
      if (!isFeasible(par, upper))
        stop("'upper' must be a feasible value!")
    }
    if (!missing(by))
      checkArg(by, "numeric", na.ok = FALSE, len)
    if (!missing(length.out)) {
      length.out = convertInteger(length.out)
      checkArg(length.out, "integer", na.ok = FALSE, len = 1L)
    }
    if (!missing(by) && !missing(length.out))
      stop("'by' and 'length.out' cannot be combined!")
    if (missing(by) && missing(length.out))
      stop("'by' or 'length.out' has to be given!")
    if (any(is.infinite(c(lower, upper))))
      stop("discretizeParam requires finite box constraints!")
    # new values
    if(!missing(by)) {
      vals = seq(from = lower, to = upper, by = by)
    } else if (!missing(length.out)) {
      vals = seq(from = lower, to = upper, length.out = length.out)
    }
    # round for integer
    if (isInteger(par))
      vals = as.integer(unique(round(vals)))
    return(makeDiscreteParam(par$id, values = vals))
  } else {
    # for discrete rest, just return
    return(par)
  }

  # supported.discrete = c("discrete", "logical", "")
  # supported.cont = c("integer", "numeric")

  # if (par$type %in% supported.discrete) {
    # par.disc = par
  # } else if (par$type %in% supported.cont) {

    # if(trafo && !is.null(par$trafo)){
      # disc.vals = par$trafo(disc.vals)
    # }
    # par.disc = makeDiscreteParam(id = par$id, values = disc.vals, trafo = par$trafo, requires = par$requires)
  # } else {
    # stop(sprintf("ParamType %s is not supported to be discretized.", par$type))
  # }
  # par.disc
}
