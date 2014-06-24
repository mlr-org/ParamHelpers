#' @rdname OptPath
#' @aliases OptPathDF
#' @export
makeOptPathDF = function(par.set, y.names, minimize, add.transformed.x = FALSE,
  include.error.message = FALSE, include.exec.time = FALSE, include.extra = FALSE) {

  checkArg(par.set, "ParamSet")
  checkArg(y.names, "character", na.ok = FALSE)
  checkArg(minimize, "logical", na.ok = FALSE)
  checkArg(add.transformed.x, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.error.message, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.exec.time, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.extra, "logical", len = 1L, na.ok = FALSE)

  n.y = length(y.names)
  obj = makeOptPath(par.set, y.names, minimize, add.transformed.x, include.error.message,
    include.exec.time, include.extra)
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
  discretes.as.factor = FALSE, include.x = TRUE, include.y = TRUE, include.rest = TRUE, ...) {

  checkArg(include.x, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.y, "logical", len = 1L, na.ok = FALSE)
  checkArg(include.rest, "logical", len = 1L, na.ok = FALSE)

  if (!include.x && !include.y && !include.rest)
    stopf("Not able to create data.frame from opt.path. You need to include something!")

  res = makeDataFrame(nrow = getOptPathLength(x), ncol = 0)

  if (include.x || include.y) {
    df = x$env$path
    y.cols = which(colnames(df) == x$y.names)
    if (include.x)
      res = cbind(res, df[, -y.cols, drop = FALSE])
    if (include.y)
      res = cbind(res, df[, y.cols, drop = FALSE])
  }
  if (include.rest) {
    res = cbind(res, dob = x$env$dob, eol = x$env$eol)
    res = convertDataFrameCols(res, chars.as.factor = discretes.as.factor)
    # if err message / exec time included, add it
    if (!is.null(x$env$error.message))
      res$error.message = x$env$error.message
    if (!is.null(x$env$exec.time))
      res$exec.time = x$env$exec.time
    if (!is.null(x$env$extra))
      res = cbind(res, convertListOfRowsToDataFrame(x$env$extra))
  }
  return(res)
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
    res$extra = e$extra[[index]]
  return(res)
}

#' @export
addOptPathEl.OptPathDF = function(op, x, y, dob = getOptPathLength(op)+1L, eol = as.integer(NA),
  error.message = NA_character_, exec.time = NA_real_, extra = NULL,
  check.feasible = !op$add.transformed.x) {

  env = op$env
  checkArg(x, "list", len = length(op$par.set$pars))
  checkArg(y, "numeric", len = length(op$y.names))
  dob = convertInteger(dob)
  checkArg(dob, "integer", len = 1L)
  eol = convertInteger(eol)
  checkArg(eol, "integer", len = 1L)
  checkArg(error.message, "character", len = 1L)
  checkArg(exec.time, "numeric", len = 1L, lower = 0, na.ok = TRUE)
  if (!is.null(extra)) {
    if (is.null(env$extra))
      stopf("Trying to add extra info to opt path, without enabling that option!")
    checkArg(extra, "list")
    if (!isProperlyNamed(extra))
      stopf("'extra' must be propely named!")
    if (!all(sapply(extra, isScalarValue)))
      stopf("'extra' can currently only contain scalar values!")
    if (length(env$extra) > 0L) {
      if (!all(names(extra) == names(env$extra[[1L]])))
        stopf("Trying to add extra with different names!")
    }
    env$extra[[length(env$extra) + 1L]] = extra
  }
  if (!is.na(error.message) && is.null(env$error.message))
    stopf("Trying to add error.message to opt path, without enabling that option!")
  if (!is.na(exec.time) && is.null(env$exec.time))
    stopf("Trying to add exec.time to opt path, without enabling that option!")

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
  colnames(el) = colnames(env$path)
  env$path = rbind(env$path, el)

  # add dob and eol
  k = length(env$dob) + 1
  env$dob[k] = dob
  env$eol[k] = eol

  # potentially add errmsg and time
  if (!is.null(env$error.message))
    env$error.message[k] = error.message
  if (!is.null(env$exec.time))
    env$exec.time[k] = exec.time

  invisible(NULL)
}


#' @export
getOptPathY.OptPathDF = function(op, names, drop = TRUE) {
  if (missing(names))
    names = op$y.names
  else
c(names, subset = op$y.names)
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

#' @export
getOptPathCols.OptPathDF = function(op, names, check.names = TRUE) {
  checkArg(names, "character")
  checkArg(check.names, "logical", len = 1L)

  df = as.data.frame(op)
  not.present.names = names[names %nin% colnames(df)]
  if (check.names) {
    if (length(not.present.names) > 0)
      stopf("You specified some cols that are not present in the opt.path: %s",
        convertToShortString(not.present.names))
  }
  present.names = setdiff(names, not.present.names)
  df[, present.names, drop = FALSE]
}

