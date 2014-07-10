#' @export
getOptPathLength.OptPathDF = function(op) {
  nrow(op$env$path)
}

#' @export
getOptPathEl.OptPathDF = function(op, index) {
  index = asInt(index)
  n = getOptPathLength(op)
  if (!(index >= 1 && index <= n))
    stop("Index must be between 1 and ", n, "!")
  e = op$env
  path = e$path
  y = unlist(path[index, op$y.names, drop = FALSE])
  # remove y names from path, only consider x
  path = path[, setdiff(colnames(path), op$y.names), drop = FALSE]
  x = dfRowToList(path, op$par.set, index)
  res = list(x = x, y = y, dob = e$dob[index], eol = e$eol[index])
  # if errmsg there, return it
  if (!is.null(e$error.message))
    res$error.message = e$error.message[index]
  if (!is.null(e$exec.time))
    res$exec.time= e$exec.time[index]
  if (!is.null(e$extra))
    res$extra = e$extra[[index]]
  return(res)
}

#' @export
getOptPathY.OptPathDF = function(op, names, drop = TRUE) {
  if (missing(names))
    names = op$y.names
  else
c(names, subset = op$y.names)
  assertFlag(drop)
  y = as.matrix(op$env$path[, names, drop = FALSE])
  if (drop && length(names) == 1L)
    y = as.numeric(y)
  return(y)
}

#' @export
getOptPathDOB.OptPathDF = function(op) {
  op$env$dob
}

#' @export
getOptPathEOL.OptPathDF = function(op) {
  op$env$eol
}

#' @export
getOptPathErrorMessages.OptPathDF = function(op) {
  op$env$error.message
}

#' @export
getOptPathExecTimes.OptPathDF = function(op) {
  op$env$exec.time
}

#' @export
getOptPathCol.OptPathDF = function(op, name) {
  assertString(name)
  if (getOptPathLength(op) == 0L)
    stopf("Trying to return a col from an empty opt.path")
  if (name %in% colnames(op$env$path))
    return(op$env$path[, name])
  if (name == "dob")
    return(getOptPathDOB(op))
  if (name == "eol")
    return(getOptPathEOL(op))
  if (name == "exec.time")
    return(getOptPathExecTimes(op))
  if (name == "error.message")
    return(getOptPathErrorMessages(op))
  if (name %in% names(op$env$extra[[1]]))
    return(extractSubList(op$env$extra, name))
  stop("The column you specified is not present in the opt.path.")
}

#' @export
getOptPathCols.OptPathDF = function(op, names, row.names = NULL) {
  assertCharacter(names, any.missing = FALSE)
  d = as.data.frame(op, row.names = row.names)
  d[, names, drop = FALSE]
}



