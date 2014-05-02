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
as.data.frame.OptPathDF = function(x, row.names = NULL, optional = FALSE,
  discretes.as.factor = FALSE, ...) {

  df = x$env$path
  df = cbind(df, dob = x$env$dob, eol = x$env$eol)
  df = convertDataFrameCols(df, chars.as.factor = discretes.as.factor)
  cbind(df, error.message = x$env$error.message, stringsAsFactors = FALSE)
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
  y = unlist(path[index, op$y.names, drop=FALSE])
  # remove y names from path, only consider x
  path = path[, setdiff(colnames(path), op$y.names), drop=FALSE]
  x = dfRowToList(path, op$par.set, index)
  list(x=x, y=y, dob=e$dob[index], eol=e$eol[index], error.message = e$error.message[index])
}

#' @S3method addOptPathEl OptPathDF
addOptPathEl.OptPathDF = function(op, x, y, dob=getOptPathLength(op)+1L, eol=as.integer(NA),
  check.feasible=!op$add.transformed.x, error.message = NA_character_) {

  checkArg(x, "list", len=length(op$par.set$pars))
  checkArg(y, "numeric", len=length(op$y.names))
  dob = convertInteger(dob)
  checkArg(dob, "integer", 1)
  eol = convertInteger(eol)
  checkArg(eol, "integer", 1)
  checkArg(error.message, "character", 1)
  if (check.feasible) {
    if(!isFeasible(op$par.set, x))
      stop("Trying to add infeasible x values to opt path: ", convertToShortString(x))
  }

  x = Map(function(p, v) {
    if (isScalarNA(v))
      v = rep(NA, p$len)
    if (p$type %in% c("discrete", "discretevector"))
      discreteValueToName(p, v)
    # we need to make sure cols in df do not get converted to num
    else if (p$type %in% c("integer", "integervector"))
      as.integer(v)
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
  op$env$error.message[k] = error.message
  invisible(NULL)
}


#' @S3method getOptPathY OptPathDF
getOptPathY.OptPathDF = function(op, names, drop=TRUE) {
  if (missing(names))
    names = op$y.names
  else
    checkArg(names, subset=op$y.names)
  checkArg(drop, "logical", len=1L, na.ok=FALSE)
  y = as.matrix(op$env$path[, names, drop=FALSE])
  if (drop && length(names) == 1L)
    y = as.numeric(y)
  return(y)
}

#' @S3method getOptPathDOB OptPathDF
getOptPathDOB.OptPathDF = function(op) {
  op$env$dob
}

#' @S3method getOptPathEOL OptPathDF
getOptPathEOL.OptPathDF = function(op) {
  op$env$eol
}

#' @S3method getOptPathErrorMessages OptPathDF
getOptPathErrorMessages.OptPathDF = function(op) {
  op$env$error.message
}






