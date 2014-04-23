#FIXME: generateGridDesign will NOT work if there are dependencies

#' Generates a grid design for a parameter set.
#'
#' @param n [\code{integer}]\cr
#'   Resolution of the grid for each numeric/integer parameter in \code{par.set}.
#'   For vector parameters, n is the resolution per dimension.
#'   Either pass one resolution for all parameters, or a named vector.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param ints.as.num [\code{logical(1)}]\cr
#'   Should parameters of type \dQuote{integer} or \dQuote{integervector} generate numeric columns?
#'   Default is \code{FALSE}.
#' @param discretes.as.factor [\code{logical(1)}]\cr
#'   Should discrete parameters have columns of type \dQuote{factor} in result?
#'   Otherwise character columns are generated.
#'   Default is \code{TRUE}.
#' @param logicals.as.factor [\code{logical(1)}]\cr
#'   Should logical parameters have columns of type \dQuote{factor} in result?
#'   Otherwise logical columns are generated.
#'   Default is \code{FALSE}.
#' @param trafo [\code{logical(1)}]\cr
#'   Transform all parameters by using theirs respective transformation functions.
#'   Default is \code{FALSE}.
#' @return [\code{data.frame.}]. Columns are named by the ids of the parameters.
#'   If the \code{par.set} argument contains a vector parameter, its corresponding column names
#'   The result will have an \code{logical(1)} attribute \dQuote{trafo},
#'   which is set to the value of argument \code{trafo}.
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("x1", lower=-2, upper=1),
#'   makeIntegerParam("x2", lower=10, upper=20, trafo = function(x) x^(-2))
#' )
#' # simple gridDesign
#' generateGridDesign(c(4,5), ps, trafo = TRUE)
generateGridDesign = function(par.set, resolution, trafo = FALSE, ints.as.num = FALSE,
  discretes.as.factor = TRUE, logicals.as.factor = FALSE) {

  checkArg(par.set, "ParamSet")
  ids = getParamIds(par.set)
  pars = par.set$pars
  n = length(pars)
  if (n == 0L)
    stop("par.set must not be empty!")
  if(any(sapply(pars, function(x) inherits(x, "LearnerParameter"))))
    stop("No par.set parameter in 'generateDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")

  m = sum(getParamLengths(par.set))
  pids1 = getParamIds(par.set)
  pids2 = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  resolution = convertIntegers(resolution)
  if (length(resolution) == 1L) {
    resolution = setNames(rep(resolution, n), pids1)
  }
  checkArg(resolution, "integer", na.ok = FALSE, lower = 1L, len = n)
  if (!isProperlyNamed(resolution) || !all(names(resolution) == pids1))
    stop("'resolution' must be named with parameter ids!")
  resolution = resolution[pids1]

  checkArg(trafo, "logical", len = 1L, na.ok = FALSE)

  vals.list = setNames(vector("list", m), pids2)
  el.counter = 1L
  for (i in 1:n) {
    p = pars[[i]]
    type = p$type
    reso = resolution[[i]]
    if (isNumeric(p)) {
      lower = p$lower
      upper = p$upper
    }
    if (isDiscrete(p, include.logical = FALSE)) {
     discvals = p$values
    }
    for (j in 1:p$len) {
      if (isDiscrete(p, include.logical = FALSE)) {
        newvals = names(discvals)
      } else if (isLogical(p)) {
        newvals = if (logicals.as.factor)
          factor(c("TRUE", "FALSE"), levels = c("TRUE", "FALSE"))
        else
          c(TRUE, FALSE)
      } else if (isNumeric(p, include.int = TRUE)) {
        newvals = seq(from = lower[[j]], to = upper[[j]], length.out = reso)
        # round for integer
        if (isInteger(p)) {
          newvals = as.integer(unique(round(newvals)))
          if (ints.as.num)
            newvals = as.numeric(newvals)
        }
      } else {
        stopf("generateGridDesign cannot be used for param '%s' of type '%s'!", p$id, p$type)
      }
      vals.list[[el.counter]] = newvals
      el.counter = el.counter + 1
    }
  }
  res = expand.grid(vals.list, KEEP.OUT.ATTRS = FALSE)
  colnames(res) = pids2
  attr(res, "trafo") = trafo
  res
}
