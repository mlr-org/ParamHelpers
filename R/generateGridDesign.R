#FIXME generateGridDesign will NOT work if there are dependencies

#' Generates a grid design for a parameter set.
#'
#' @param n [\code{integer(k)}]\cr
#'   Resolution of the grid for each ot the \code{k} parameters in the \code{par.set}.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param trafo [\code{logical(1)}]\cr
#'   Transform all parameters by using theirs respective transformation functions.
#'   Default is \code{FALSE}.
#' @return The created design is a data.frame. Columns are named by the ids of the parameters.
#'   If the \code{par.set} argument contains a vector parameter, its corresponding column names
#'   The result will have an \code{logical(1)} attribute \dQuote{trafo},
#'   which is set to the value of argument \code{trafo}.
#' @export
#' @examples
#' ps <- makeParamSet(
#'   makeNumericParam("x1", lower=-2, upper=1),
#'   makeIntegerParam("x2", lower=10, upper=20, trafo = function(x) x^(-2))
#' )
#' # simple gridDesign
#' generateGridDesign(c(4,5), ps, trafo = TRUE)
generateGridDesign = function(n=10L, par.set, trafo=FALSE) {
  n = convertIntegers(n)
  checkArg(n, "integer", na.ok=FALSE)
  checkArg(par.set, "ParamSet")
  checkArg(trafo, "logical", len=1L, na.ok=FALSE)
  
  if (length(par.set$pars) == 0)
    stop("par.set must not be empty!")
  if(any(sapply(par.set$pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'generateDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
  ids = getParamIds(par.set)
  pars = par.set$pars
  if(length(n) == 1)
    n = rep(n, length(pars))
  if(is.null(names(n)))
    names(n) = ids 
  disc.pars = lapply(ids, function(id) {
    discretizeParam(pars[[id]], length.out=n[id], trafo=trafo)
  })
  disc.values = lapply(disc.pars, function(x) unlist(x$values))
  res = expand.grid(disc.values)
  colnames(res) = ids
  attr(res, "trafo") = trafo
  res
}