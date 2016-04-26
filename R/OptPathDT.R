#' @rdname OptPath
#' @aliases OptPathDT
#' @export
makeOptPathDT = function(par.set, y.names, minimize, add.transformed.x = FALSE) {

  assertClass(par.set, "ParamSet")
  assertCharacter(y.names)
  assertLogical(minimize)
  assertFlag(add.transformed.x)

  n.y = length(y.names)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x, include.error.message,
    include.exec.time, include.extra)
  col.types = c("integer", "integer", getParamTypes(par.set, df.cols = TRUE, df.discretes.as.factor = FALSE), rep("numeric", n.y))
  col.names = c("dob", "eol", getParSetNamesForTable(par.set), y.names)
  opdt = as.data.table(lapply(col.types, vector, length = 0))
  colnames(opdt) = col.names
  obj$env$opdt = opdt
  return(addClasses(obj, "OptPathDT"))
}

getParSetNamesForTable = function(par.set) {
  getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
}

getOptPathExtraNames = function(opt.path) {
  setdiff(colnames(opt.path$evn$opdt), c("dob", "eol", getParSetNamesForTable(opt.path$par.set), opt.path$y.names))
}