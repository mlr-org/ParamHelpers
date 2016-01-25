addOptPathEl.OptPathDF = function(op, x, y, dob = getOptPathLength(op)+1L, eol = NA_integer_,
  error.message = NA_character_, exec.time = NA_real_, extra = NULL,
  check.feasible = !op$add.transformed.x) {

  env = op$env
  assertList(x, len = length(op$par.set$pars))
  assertNumeric(y, len = length(op$y.names))
  dob = asInt(dob, na.ok = TRUE)
  eol = asInt(eol, na.ok = TRUE)
  assertString(error.message, na.ok = TRUE)
  assertNumber(exec.time, lower = 0, na.ok = TRUE)

  if (!is.null(extra)) {
    if (is.null(env$extra))
      stopf("Trying to add extra info to opt path, without enabling that option!")
    assertList(extra)
    if (!isProperlyNamed(extra))
      stopf("'extra' must be properly named!")
    if (!all(sapply(extra, isScalarValue)))
      stopf("'extra' can currently only contain scalar values!")
    if (length(env$extra) > 0L) {
      if (!all(names(extra) == names(env$extra[[1L]])))
        stopf("Trying to add unknown extra(s): %s!", paste(symdiff(names(extra), names(env$extra[[1L]])), collapse = ","))
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
        v = getParamNA(p, repeated = TRUE)
      else if (p$type %in% c("discrete", "discretevector"))
        discreteValueToName(p, v)
      # we need to make sure cols in df do not get converted to num
      else if (p$type %in% c("integer", "integervector"))
        as.integer(v)
      else
        v
    }, ps$pars, x)
  }

  # print(str(x))
  x = recode(op$par.set, x)
  # print(str(x))

  if (nrow(env$path) == env$path.len) {
    env$path = rbind(env$path, env$path)
    if (!is.null(error.message))
      env$error.message = c(env$error.message, env$error.message)
    if (!is.null(exec.time))
      env$exec.time = c(env$exec.time, env$exec.time)
    if (!is.null(env$extra))
      env$extra = c(env$extra, env$extra)
  }

  .Call("c_addOptPathDF", env$path, env$path.len, x, y,
    env$dob, dob, env$eol, eol, PACKAGE = "ParamHelpers")

  env$path.len = env$path.len + 1L

  # add dob and eol
  k = length(env$dob) + 1
  env$dob[k] = dob
  env$eol[k] = eol

  # potentially add errmsg and time
  if (!is.null(env$error.message))
    env$error.message[k] = error.message
  if (!is.null(env$exec.time))
    env$exec.time[k] = exec.time

  return(invisible(NULL))
}





