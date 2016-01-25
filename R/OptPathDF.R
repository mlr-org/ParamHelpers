#' @rdname OptPath
#' @aliases OptPathDF
#' @export
makeOptPathDF = function(par.set, y.names, minimize, add.transformed.x = FALSE,
  include.error.message = FALSE, include.exec.time = FALSE, include.extra = FALSE, nrow = 500L) {

  assertClass(par.set, "ParamSet")
  assertCharacter(y.names)
  assertLogical(minimize)
  assertFlag(add.transformed.x)
  assertFlag(include.error.message)
  assertFlag(include.exec.time)
  assertFlag(include.extra)
  n.y = length(y.names)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x, include.error.message,
    include.exec.time, include.extra)
  obj$env$path = makeDataFrame(nrow = nrow, ncol = getParamNr(par.set, devectorize = TRUE) + n.y,
    col.types = c(getParamTypes(par.set, df.cols = TRUE, df.discretes.as.factor = FALSE), rep("numeric", n.y)),
    col.names = c(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), y.names)
  )
  obj$env$path.len = 0L
  return(addClasses(obj, "OptPathDF"))
}


