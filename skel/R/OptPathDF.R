#' @rdname OptPath
#' @aliases OptPathDF
#' @export
makeOptPathDF = function(par.set, y.names, minimize, add.transformed.x=FALSE) {
  checkArg(par.set, "ParamSet")
  checkArg(y.names, "character", na.ok=FALSE)
  checkArg(minimize, "logical", na.ok=FALSE)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x)
  ns = c(getParamIds(par.set, repeated=TRUE, with.nr=TRUE), y.names)
  obj$env$path = as.data.frame(matrix(0, nrow=0, ncol=length(ns)))
  colnames(obj$env$path) = ns
  class(obj) = c("OptPathDF", class(obj))
  return(obj)
}

#' @S3method getOptPathLength OptPathDF
getOptPathLength.OptPathDF = function(op) {
  nrow(op$env$path)
}

#' @S3method as.data.frame OptPathDF
as.data.frame.OptPathDF = function(x, row.names = NULL, optional = FALSE, discretes.as.factor = FALSE, ...) {
  df = x$env$path
  df = cbind(df, dob=x$env$dob, eol=x$env$eol)
  convertDfCols(df, chars.as.factor=discretes.as.factor)
}

#' @S3method getOptPathEl OptPathDF
getOptPathEl.OptPathDF = function(op, index) {
  index = convertInteger(index)
  checkArg(index, "integer", 1)
  n = getOptPathLength(op)
  if (!(index >= 1 && index <= n))
    stop("Index must be between 1 and ", n, "!")
  e = op$env
  path = e$path
  x = dfRowToList(path, op$par.set, index)
  y = unlist(path[index, op$y.names, drop=FALSE])
  list(x=x, y=y, dob=e$dob[index], eol=e$eol[index])
}

#' @S3method addOptPathEl OptPathDF
addOptPathEl.OptPathDF = function(op, x, y, dob=getOptPathLength(op)+1L, eol=as.integer(NA), check.feasible=!op$add.transformed.x) {
  checkArg(x, "list", length(op$par.set$pars))
  checkArg(y, "numeric", length(op$y.names))
  dob = convertInteger(dob)
  checkArg(dob, "integer", 1)
  eol = convertInteger(eol)
  checkArg(eol, "integer", 1)
  if (check.feasible) {
    if(!isFeasible(op$par.set, x))
      stop("Trying to add infeasible x values to opt path: ", listToShortString(x))
  }
  x = Map(function(p, v) {
    if (isScalarNA(v))
      v = rep(NA, p$len)
    if (p$type %in% c("discrete", "discretevector")) 
      discreteValueToName(p, v) 
    else 
      v
  }, op$par.set$pars, x)  
  el = do.call(cbind, lapply(x, function(v) as.data.frame(t(v), stringsAsFactors=FALSE)))
  el = cbind(el, as.data.frame(as.list(y), stringsAsFactors=FALSE))
  colnames(el) = colnames(op$env$path)
  op$env$path = rbind(op$env$path, el)
  k = length(op$env$dob) + 1
  op$env$dob[k] = dob  
  op$env$eol[k] = eol
  invisible(NULL)
}


#' @S3method getOptPathY OptPathDF
getOptPathY.OptPathDF = function(op, name) {
  checkArg(name, choices=op$y.names)
  op$env$path[, name]
}  

#' @S3method getOptPathDOB OptPathDF
getOptPathDOB.OptPathDF = function(op) {
  op$env$dob
}  

#' @S3method getOptPathEOL OptPathDF
getOptPathEOL.OptPathDF = function(op) {
  op$env$eol
}  






