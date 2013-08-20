#' Generates a statistical design for a parameter set.
#'
#' Currently only lhs designs are supported.
#'
#' @param n [\code{integer(1)}]\cr 
#'   Number of samples in design.   
#'   Default is 10.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param fun [\code{function}]\cr
#'   Function from package lhs. 
#'   Possible are: \code{\link[lhs]{maximinLHS}}, \code{\link[lhs]{randomLHS}}, \code{\link[lhs]{geneticLHS}}, \code{\link[lhs]{improvedLHS}}, \code{\link[lhs]{optAugmentLHS}}, \code{\link[lhs]{optimumLHS}} 
#'   Default is \code{\link[lhs]{randomLHS}}.
#' @param fun.args [\code{list}]\cr
#'   List of further arguments passed to \code{fun}. 
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
#' generateDesign(5, ps)
#' 
#' # with trafo 
#' ps <- makeParamSet(
#'   makeNumericParam("x", lower=-2, upper=1), 
#'   makeNumericVectorParam("y", len=2, lower=0, upper=1, trafo=function(x) x/sum(x)) 
#' )
#' generateDesign(10, ps, trafo=TRUE)
generateDesign = function(n=10L, par.set, fun, fun.args=list(), trafo=FALSE, ints.as.num=FALSE, discretes.as.factor=TRUE, logicals.as.factor=FALSE) {
  n = convertInteger(n)
  checkArg(n, "integer", len=1L, na.ok=FALSE)
  checkArg(par.set, "ParamSet")
  requirePackages("lhs", "generateDesign")
  if (missing(fun))  
    fun = lhs::randomLHS
  else  
    checkArg(fun, "function")
  checkArg(fun.args, "list")
  checkArg(trafo, "logical", len=1L, na.ok=FALSE)
  checkArg(ints.as.num, "logical", len=1L, na.ok=FALSE)
  checkArg(discretes.as.factor, "logical", len=1L, na.ok=FALSE)
  
  if (length(par.set$pars) == 0)
    stop("par.set must not be empty!")        
  if(any(sapply(par.set$pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'generateDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")        
  lower = getLower(par.set)
  upper = getUpper(par.set)
  
  if (any(is.infinite(c(lower, upper))))
    stop("generateDesign requires finite box constraints!")
  
  pars = par.set$pars
  
  k = sum(getParamLengths(par.set))
  des = do.call(fun, c(list(n=n, k=k), fun.args))
  des = as.data.frame(des)
  
  col = 0
  for (i in 1:length(pars)) {
    p = pars[[i]]
    cc = rev(col)[1]
    if (p$type %in% c("numericvector", "integervector", "discretevector", "logicalvector")) 
      col = (cc + 1) : (cc + p$len)   
    else 
      col = cc + 1    
    trafo.fun = if (trafo && !is.null(p$trafo)) p$trafo else identity
    if (p$type == "numeric")
      v = (p$upper-p$lower)*des[,col] + p$lower
    else if (p$type == "integer") {
      v = as.integer(floor((p$upper-p$lower+1)*des[,col] + p$lower))
    } else if (p$type == "numericvector") {
      v = t((p$upper-p$lower)*t(des[,col]) + p$lower)
    } else if (p$type == "integervector") {
      v = floor((p$upper-p$lower+1)*as.matrix(des[,col]) + p$lower)
      if (!ints.as.num)
        mode(v) = "integer"
    } else if (p$type %in% c("logical", "logicalvector"))
      v = ifelse(des[,col] <= 0.5, FALSE, TRUE)
    else if (p$type %in% c("discrete", "discretevector")) {
      ns = names(p$values)
      indices = ceiling(des[,col,drop=FALSE] * length(ns))
      v = lapply(1:ncol(indices), function(i) {
        ns[indices[,i]]
      })
      v = do.call(function(...) data.frame(..., stringsAsFactors=FALSE), v)
    }
    
    # trafo
    if (trafo) {
      if (p$type %in% c("numeric", "integer"))
        v = trafo.fun(v)
      else if (p$type %in% c("numericvector", "integervector"))
        v = t(apply(v, 1, trafo.fun))
    }
    des[, col] = v
  }
  
  #FIXME: *very* inefficient
  vals = dfRowsToList(des, par.set)
  for (i in 1:length(vals)) {
    v = vals[[i]]
    col = 0
    for (j in seq_along(par.set$pars)) {
      p = par.set$pars[[j]]
      cc = rev(col)[1]
      if (p$type %in% c("numericvector", "integervector", "discretevector", "logicalvector")) 
        col = (cc + 1) : (cc + p$len)   
      else 
        col = cc + 1      
      if (!is.null(p$requires) && !requiresOk(par.set, v, j)) {
        des[i, col] = NA
      }
    }
  }
  
  colnames(des) = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  des = convertDfCols(des, ints.as.num=ints.as.num, chars.as.factor=discretes.as.factor, logicals.as.factor=logicals.as.factor)
  attr(des, "trafo") = trafo
  return(des)
}
