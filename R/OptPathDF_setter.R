#' @export
addOptPathEl.OptPathDF = function(op, x, y, dob = getOptPathLength(op)+1L, eol = NA_integer_,
  error.message = NA_character_, exec.time = NA_real_, extra = NULL,
  check.feasible = !op$add.transformed.x) {

  env = op$env
  assertList(x, len = length(op$par.set$pars))
  assertNumeric(y, len = length(op$y.names))
  eol = asInt(eol, na.ok = TRUE)
  dob = asInt(dob, na.ok = TRUE)
  assertString(error.message, na.ok = TRUE)
  assertNumber(exec.time, lower = 0, na.ok = TRUE)
  if (!is.null(env$extra) && (is.null(extra) || length(extra) == 0))
    stopf("You have to add extras to opt path if the option is enabled.")
  if (!is.null(extra)) {
    if (is.null(env$extra))
      stopf("Trying to add extra info to opt path, without enabling that option!")
    assertList(extra)
    if (!isProperlyNamed(extra))
      stopf("'extra' must be properly named!")
    nondot.extra = removeDotEntries(extra)
    if (!all(vlapply(nondot.extra, isScalarValue)))
      stopf("'nondot' components of 'extra' can only contain scalar values! use the 'dotted' components for more complex objects!")
    if (length(env$extra) > 0L) {
      nondot.extra.precedent = removeDotEntries(env$extra[[1L]])
      unknown.extra = setdiff(names(nondot.extra), names(nondot.extra.precedent))
      missing.extra = setdiff(names(nondot.extra.precedent), names(nondot.extra))
      if (length(unknown.extra) > 0)
        stopf("Trying to add unknown extra(s): %s!", collapse(unknown.extra))
      if (length(missing.extra) > 0)
        stopf("Trying to add extras but missing: %s!", collapse(missing.extra))
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

  return(invisible(NULL))
}
