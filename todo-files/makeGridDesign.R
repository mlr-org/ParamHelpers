#FIXME generateDesign will NOT work if there are dependencies
# over multiple levels of params and one only states the dependency only
#  wrt to the "last" param. also see daniels unit test.
#  it works as long all dependencies are stated, we need to at least document this


#' Generates a grid design for a parameter set.
#'
#' @param n [\code{integer(k)}]\cr
#'   Resolution of the grid for each ot the \code{k} parameters in the \code{par.set}.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param trafo [\code{logical(1)}]\cr
#'   Transform all parameters by using theirs respective transformation functions.
#'   Default is \code{FALSE}.
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
#' @return The created design is a data.frame. Columns are named by the ids of the parameters.
#'   If the \code{par.set} argument contains a vector parameter, its corresponding column names
#'   in the design are the parameter id concatenated with 1 to dimension of the vector.
#'   The data type of a column
#'   is defined in the following way. Numeric parameters generate numeric columns, integer parameters generate numeric/integer columns,
#'   logical parameters generate logical/factor columns.
#'   For discrete parameters the value names are used and character or factor columns are generated.
#'   Dependent parameters whose constaints are unsatisfied generate \code{NA} entries in their
#'   respective columns.
#'   The result will have an \code{logical(1)} attribute \dQuote{trafo},
#'   which is set to the value of argument \code{trafo}.
#' @export
#' @examples
#' ps <- makeParamSet(
#'   makeNumericParam("x1", lower=-2, upper=1),
#'   makeIntegerParam("x2", lower=10, upper=20)
#' )
#' # random latin hypercube design with 5 samples:
#' generateGridDesign(c(5,5), ps)
#'
#' # with trafo
#' ps <- makeParamSet(
#'   makeNumericParam("x", lower=-2, upper=1),
#'   makeNumericVectorParam("y", len=2, lower=0, upper=1, trafo=function(x) x/sum(x))
#' )
#' generateGridDesign(c(10,10), ps, trafo=TRUE)
generateGridDesign = function(n=10L, par.set, fun, fun.args=list(), trafo=FALSE, ints.as.num=FALSE, discretes.as.factor=TRUE, logicals.as.factor=FALSE) {
  n = convertIntegers(n)
  checkArg(n, "integer", na.ok=FALSE)
  checkArg(par.set, "ParamSet")
  checkArg(trafo, "logical", len=1L, na.ok=FALSE)
  checkArg(ints.as.num, "logical", len=1L, na.ok=FALSE)
  checkArg(discretes.as.factor, "logical", len=1L, na.ok=FALSE)

  if (length(par.set$pars) == 0)
    stop("par.set must not be empty!")
  if(any(sapply(par.set$pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'generateDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
  ids = getParamIds(par.set)
  lower = getLower(par.set, with.nr=TRUE)
  upper = getUpper(par.set, with.nr=TRUE)
  values = getValues(par.set)
  types = getTypes(par.set, use.names=TRUE)

  if (any(is.infinite(c(lower, upper))))
    stop("generateGridDesign requires finite box constraints!")

  pars = par.set$pars
  pids1 = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  pids2 = getParamIds(par.set, repeated=TRUE, with.nr=FALSE)
  lens = getParamLengths(par.set)
  k = sum(lens)
  
  if(is.null(names(n))){
    names(n) = ids 
  }

  gridpoints = lapply(ids, function(id) {
    if(types[id] %in% c("numeric","integer")){
      res = seq(from=lower[id], to=upper[id], length.out=n[id])
    }else{
      res = unlist(values[[id]])
    }
    res
  })
  des = expand.grid(gridpoints)

  res = des

  attr(res, "trafo") = trafo
  return(res)
}