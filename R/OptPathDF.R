#' @rdname OptPath
#' @aliases OptPathDF
#' @export
makeOptPathDF = function(par.set, y.names, minimize, add.transformed.x = FALSE,
  include.error.message = FALSE, include.exec.time = FALSE, extra.par.set = NULL) {

  checkArg(par.set, "ParamSet")
  checkArg(y.names, "character", na.ok = FALSE)
  checkArg(minimize, "logical", na.ok = FALSE)
  checkArg(add.transformed.x, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.error.message, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.exec.time, "logical", len = 1L, na.ok = FALSE)

  n.y = length(y.names)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x, include.error.message,
    include.exec.time, extra.par.set)
  obj$env$path = makeDataFrame(nrow = 0, ncol = getParamNr(par.set, devectorize = TRUE) + n.y,
    col.types = c(getParamTypes(par.set, df.cols = TRUE, df.discretes.as.factor = FALSE), rep("numeric", n.y)),
    col.names = c(getParamIds(par.set, repeated = TRUE, with.nr = TRUE), y.names)
  )
  addClasses(obj, "OptPathDF")
}

#' @export
getOptPathLength.OptPathDF = function(op) {
  nrow(op$env$path)
}

#' @export
as.data.frame.OptPathDF = function(x, row.names = NULL, optional = FALSE,
  discretes.as.factor = FALSE, ...) {

  df1 = x$env$path
  df1 = cbind(df1, dob = x$env$dob, eol = x$env$eol)
  df1 = convertDataFrameCols(df1, chars.as.factor = discretes.as.factor)
  # if err message / exec time included, add it
  if (!is.null(x$env$error.message))
    df1$error.message = x$env$error.message
  if (!is.null(x$env$exec.time))
    df1$exec.time = x$env$exec.time
  if (!is.null(x$env$extra)) {
    df2 = x$env$extra
    df2 = convertDataFrameCols(df2, chars.as.factor = discretes.as.factor)
    df1 = cbind(df1, df2)
  }
  return(df1)
}

#' @export
getOptPathEl.OptPathDF = function(op, index) {
  index = convertInteger(index)
  checkArg(index, "integer", 1)
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
    res$extra = dfRowToList(e$extra, op$extra.par.set, index)
  return(res)
}

#' @export
addOptPathEl.OptPathDF = function(op, x, y, dob = getOptPathLength(op)+1L, eol = as.integer(NA),
  error.message = NA_character_, exec.time = NULL, extra = NULL,
  check.feasible = !op$add.transformed.x) {

  checkArg(x, "list", len = length(op$par.set$pars))
  checkArg(y, "numeric", len = length(op$y.names))
  dob = convertInteger(dob)
  checkArg(dob, "integer", len = 1L)
  eol = convertInteger(eol)
  checkArg(eol, "integer", len = 1L)
  checkArg(error.message, "character", len = 1L)
  if (!is.null(exec.time))
    checkArg(exec.time, "numeric", len = 1L, lower = 0, na.ok = TRUE)
  if (!is.null(extra))
    checkArg(extra, "list", len = length(op$extra.par.set$pars))

  if (!is.na(error.message) && is.null(op$env$error.message))
    stopf("Trying to add error.message to opt path, without enabling that option!")
  if (!is.null(exec.time) && is.null(op$env$exec.time))
    stopf("Trying to add exec.time to opt path, without enabling that option!")
  if (!is.null(extra) && is.null(op$extra.par.set))
    stopf("Trying to add extra info to opt path, without enabling that option!")

  if (check.feasible) {
    if (!isFeasible(op$par.set, x))
      stop("Trying to add infeasible x values to opt path: ", convertToShortString(x))
  }

  # scalar_na -> single_NA, disc --> names, ints --> make sure int
  recode = function(ps, x)  {
    Map(function(p, v) {
      if (isScalarNA(v))
        v = rep(NA, p$len)
      if (p$type %in% c("discrete", "discretevector"))
        discreteValueToName(p, v)
      # we need to make sure cols in df do not get converted to num
      else if (p$type %in% c("integer", "integervector"))
        as.integer(v)
      else
        v
    }, ps$pars, x)
  }


  # add x and y
  x = recode(op$par.set, x)
  el = do.call(cbind, lapply(x, function(v) as.data.frame(t(v), stringsAsFactors = FALSE)))
  el = cbind(el, as.data.frame(as.list(y), stringsAsFactors = FALSE))
  colnames(el) = colnames(op$env$path)
  op$env$path = rbind(op$env$path, el)

  # add dob and eol
  k = length(op$env$dob) + 1
  op$env$dob[k] = dob
  op$env$eol[k] = eol

  # potentially add errmsg and time
  if (!is.null(op$env$error.message))
    op$env$error.message[k] = error.message
  if (!is.null(op$env$exec.time))
    op$env$exec.time[k] = exec.time

  # potentially add extra
  if (!is.null(op$extra.par.set)) {
    extra = recode(op$extra.par.set, extra)
    el = do.call(cbind, lapply(extra, function(v) as.data.frame(t(v), stringsAsFactors = FALSE)))
    colnames(el) = colnames(op$env$extra)
    op$env$extra = rbind(op$env$extra, el)
  }
  invisible(NULL)
}


#' @export
getOptPathY.OptPathDF = function(op, names, drop = TRUE) {
  if (missing(names))
    names = op$y.names
  else
    checkArg(names, subset = op$y.names)
  checkArg(drop, "logical", len = 1L, na.ok = FALSE)
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

