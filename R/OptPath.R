#' @title Create optimization path.
#'
#' @description
#' Optimizers can iteratively log their evaluated points
#' into this object. Can be converted into a data.frame with
#' \code{as.data.frame(x, discretes.as.factor = TRUE / FALSE)}.
#'
#' A optimization path has a number of path elements, where each element consists of: the value of the
#' decision variables at this point, the values of the performance measures at this point,
#' the date-of-birth (dob) of this point, the end-of-life (eol) of this point and possibly
#' an error message. See also \code{\link{addOptPathEl}}.
#'
#' For discrete parameters always the name of the value is stored as a character.
#' When you retrieve an element with \code{\link{getOptPathEl}}, this name is converted to
#' the actual discrete value.
#'
#' If parameters have associated transformation you are free to decide whether you want to
#' add x values before or after transformation, see argument \code{add.transformed.x} and
#' \code{\link{trafoOptPath}}.
#'
#' The S3 class is a list which stores at least these elements:
#' \describe{
#' \item{par.set [\code{\link{ParamSet}}]}{See argument of same name.}
#' \item{y.names [\code{character}]}{See argument of same name.}
#' \item{minimize [\code{logical}]}{See argument of same name.}
#' \item{add.transformed.x [\code{logical(1)}]}{See argument of same name.}
#' \item{env [\code{environment}]}{Environment which stores the optimization path.
#'   Contents depend on implementation.}
#' }
#'
#' @template arg_parset
#' @param y.names [\code{character}]\cr
#'   Names of performance measures that are optimized or logged.
#' @param minimize [\code{logical}]\cr
#'   Which of the performance measures in y.names should be minimized?
#'   Vector of booleans in the same order as \code{y.names}.
#' @param add.transformed.x [\code{logical(1)}]\cr
#'   If some parameters have associated transformations, are you going to
#'   add x values after they have been transformed?
#'   Default is \code{FALSE}.
#' @param include.error.message [\code{logical(1)}]\cr
#'   Should it be possible to include an error message string (or NA if no error occurred)
#'   into the path for each evaluation?
#'   This is useful if you have complex, long running objective evaluations that might fail.
#'   Default is \code{FALSE}.
#' @param include.exec.time [\code{logical(1)}]\cr
#'   Should it be possible to include execution time of evaluations
#'   into the path for each evaluation?
#'   Note that execution time could also be entered in \code{y.names} as a direct
#'   performance measure. If you use this option here, time is regarded as an extra measurement
#'   you might be curious about.
#'   Default is \code{FALSE}.
#' @param include.extra [\code{logical(1)}]\cr
#'   Should it be possible to include extra info
#'   into the path for each evaluation?
#'   Default is \code{FALSE}.
#' @name OptPath
#' @rdname OptPath
#' @family optpath
NULL

makeOptPath = function(par.set, y.names, minimize, add.transformed.x = FALSE,
  include.error.message = FALSE, include.exec.time = FALSE, include.extra = FALSE) {

  n.y = length(y.names)
  ok = c("numeric", "integer", "numericvector", "integervector", "logical",
    "logicalvector", "discrete", "discretevector")
  if(length(par.set$pars) > length(filterParams(par.set, ok)$pars))
    stop("OptPath can currently only be used for: ", paste(ok, collapse = ","))
  x.names = getParamIds(par.set)
  # be really sure that x and y columns are uniquely named
  x.names2 = c(getParamIds(par.set, with.nr = TRUE), getParamIds(par.set, with.nr = FALSE))
  if (length(intersect(x.names2, y.names)) > 0)
    stop("'x.names' and 'y.names' must not contain common elements!")
  if (length(minimize) != n.y)
    stop("'y.names' and 'minimize' must be of the same length!")
  if (is.character(names(minimize)) && !setequal(names(minimize), y.names))
    stop("Given names for 'minimize' must be the same as 'y.names'!")
  if (is.null(names(minimize)))
    names(minimize) = y.names
  if (any(c("dob", "eol", "error.message") %in% (union(x.names, y.names))))
    stop("'dob', 'eol' and 'error.message' are not allowed in parameter names or 'y.names'!")
  ee = new.env()
  ee$dob = ee$eol = integer(0)

  # potentially init error.message and exec.time in env
  ee$error.message = if (include.error.message) character(0L) else NULL
  ee$exec.time = if (include.exec.time) numeric(0L) else NULL
  ee$extra = if (include.extra) list() else NULL

  makeS3Obj("OptPath",
    par.set = par.set,
    y.names = y.names,
    minimize = minimize,
    add.transformed.x = add.transformed.x,
    env = ee
  )
}

#' @export
print.OptPath = function(x, ...) {
  n = getOptPathLength(x)
  em = x$env$error.message
  et = x$env$exec.time
  ex = x$env$extra
  catf("Optimization path")
  catf("  Dimensions: x = %i/%i, y = %i",
    length(x$par.set$pars), sum(getParamLengths(x$par.set)), length(x$y.names))
  catf("  Length: %i", n)
  catf("  Add x values transformed: %s", x$add.transformed.x)
  s = if (is.null(em)) ""  else sprintf(" Errors: %i / %i.", sum(!is.na(em)), n)
  catf("  Error messages: %s.%s", !is.null(em), s)
  s = if (is.null(et)) ""  else sprintf(" Range: %g - %g.", min(et, na.rm = TRUE), max(et, na.rm = TRUE))
  catf("  Exec times: %s.%s", !is.null(et), s)
  if (!is.null(ex))
  catf("  Extras: %i columns", ifelse(length(ex) > 0L, length(ex[[1]]), NA))
}


#' Get the length of the optimization path.
#'
#' Dependent parameters whose requirements are not satisfied are represented by a scalar
#' NA in the output.
#'
#' @template arg_op
#' @return [\code{integer(1)}]
#' @export
#' @family optpath
getOptPathLength = function(op) {
  UseMethod("getOptPathLength")
}

#' Get an element from the optimization path.
#'
#' Dependent parameters whose requirements are not satisfied are represented by a scalar NA
#' in the elements of \code{x} of the return value.
#'
#' @template arg_op
#' @param index [\code{integer(1)}]\cr
#'   Index of element.
#' @return List with elements \code{x} [named \code{list}], \code{y} [named \code{numeric}],
#'   \code{dob} [\code{integer(1)}], \code{eol} [\code{integer(1)}].
#'   The elements \code{error.message} [\code{character(1)}],
#'   \code{exec.time} [\code{numeric(1)}] and \code{extra} [named \code{list}] are
#'   there if the respective options in \code{\link{OptPath}} are enabled.
#' @rdname getOptPathEl
#' @export
#' @family optpath
getOptPathEl = function(op, index) {
  UseMethod("getOptPathEl")
}

#' Add a new element to an optimization path.
#'
#' Changes the argument in-place.
#' Note that when adding parameters that have associated tranformations, it is probably
#' best to add the untransformed values to the path. Otherwise you have to switch off the
#' feasibility check, as constraints might now not hold anymore.
#'
#' Dependent parameters whose requirements are not satisfied must be represented by a scalar
#' NA in the input.
#'
#' @template arg_op
#' @param x [\code{list}]\cr
#'   List of parameter values for a point in input space. Must be in same order as parameters.
#' @param y [\code{numeric}]\cr
#'   Vector of fitness values.  Must be in same order as \code{y.names}.
#' @param dob [\code{integer(1)}]\cr
#'   Date of birth of the new parameters.
#'   Default is length of path + 1.
#' @param eol [\code{integer(1)}]\cr
#'   End of life of point.
#'   Default is \code{NA}.
#' @param error.message [\code{character(1)}]\cr
#'   Possible error message that occurred for this parameter values.
#'   Default is \code{NA}.
#' @param exec.time [\code{numeric(1)}]\cr
#'   Possible exec time for this evaluation.
#'   Default is \code{NA}.
#' @param extra [\code{list}]\cr
#'   Possible list of extra values to store.
#'   Must be in same order as in \code{extra.par.set} of \code{\link{OptPath}}.
#'   Default is \code{NULL}
#' @param check.feasible [\code{logical(1)}]\cr
#'   Should \code{x} be checked with \code{\link{isFeasible}}?
#'   Default is \code{TRUE}.
#' @return Nothing.
#' @export
#' @family optpath
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("p1"),
#'   makeDiscreteParam("p2", values = c("a", "b"))
#' )
#' op = makeOptPathDF(par.set = ps, y.names = "y", minimize = TRUE)
#' addOptPathEl(op, x = list(p1 = 7, p2 = "b"), y = 1)
#' addOptPathEl(op, x = list(p1 = -1, p2 = "a"), y = 2)
#' as.data.frame(op)
addOptPathEl = function(op, x, y, dob = getOptPathLength(op)+1L, eol = as.integer(NA),
  error.message = NA_character_, exec.time = NA_real_, extra = NULL,
  check.feasible = !op$add.transformed.x) {

  UseMethod("addOptPathEl")
}

#' Get index of the best element from optimization path.
#'
#' @template arg_op
#' @param y.name [\code{character(1)}]\cr
#'   Name of target value to decide which element is best.
#'   Default is \code{y.names[1]}.
#' @param dob [\code{integer}]\cr
#'   Possible dates of birth to select best element from. Defaults to all.
#' @param eol [\code{integer}]\cr
#'   Possible end of life to select best element from. Defaults to all.
#' @param ties [\code{character(1)}]\cr
#'   How should ties be broken when more than one optimal element is found?
#'   \dQuote{all}: return all indices,
#'   \dQuote{first}: return first optimal element in path,
#'   \dQuote{last}: return last optimal element in path,
#'   \dQuote{random}: return random optimal element in path.
#'   Default is \dQuote{last}.
#' @return [\code{integer}]
#'   Index or indices into path. See \code{ties}.
#' @export
#' @family optpath
#' @examples
#' ps = makeParamSet(makeNumericParam("x"))
#' op = makeOptPathDF(par.set = ps, y.names = "y", minimize = TRUE)
#' addOptPathEl(op, x = list(x = 1), y = 5)
#' addOptPathEl(op, x = list(x = 2), y = 3)
#' addOptPathEl(op, x = list(x = 3), y = 9)
#' addOptPathEl(op, x = list(x = 4), y = 3)
#' as.data.frame(op)
#' getOptPathBestIndex(op)
#' getOptPathBestIndex(op, ties = "first")
getOptPathBestIndex = function(op, y.name = op$y.names[1], dob = op$env$dob, eol = op$env$eol, ties = "last") {
  assertClass(op, "OptPath")
  assertChoice(y.name, choices = op$y.names)
  dob = asInteger(dob, any.missing = TRUE)
  eol = asInteger(eol, any.missing = TRUE)
  assertChoice(ties, c("all", "first", "last", "random"))
  life.inds = which(op$env$dob %in% dob & op$env$eol %in% eol)
  if (length(life.inds) == 0)
    stop("No element found which matches dob and eol restrictions!")
  y = getOptPathY(op, y.name)[life.inds]
  if (all(is.na(y))) {
    best.inds = life.inds
  } else {
    if (op$minimize[y.name])
      best.inds = which(min(y, na.rm = TRUE) == y)
    else
      best.inds = which(max(y, na.rm = TRUE) == y)
    best.inds = life.inds[best.inds]
  }
  if (length(best.inds) > 1) {
    if (ties == "all")
      return(best.inds)
    else if (ties == "first")
      return(best.inds[1])
    else if (ties == "last")
      return(best.inds[length(best.inds)])
    else if (ties == "random")
      return(best.inds[sample(length(best.inds), 1)])
  } else {
    return(best.inds)
  }
}

#' Get indices of pareto front of optimization path.
#'
#' @template arg_op
#' @param y.names [\code{character}]\cr
#'   Names of performance measures to construct pareto front for.
#'   Default is all performance measures.
#' @param dob [\code{integer}]\cr
#'   Possible dates of birth to select elements from. Defaults to all.
#' @param eol [\code{integer}]\cr
#'   Possible end of life to select elements from. Defaults to all.
#' @param index [\code{logical(1)}]\cr
#'   Return indices into path of front or y-matrix of nondominated points?
#'   Default is \code{FALSE}.
#' @return [\code{matrix} | \code{integer}]. Either matrix (with named columns) of points of front
#'   in objective space or indices into path for front.
#' @export
#' @family optpath
#' @examples
#' ps = makeParamSet(makeNumericParam("x"))
#' op = makeOptPathDF(par.set = ps, y.names = c("y1", "y2"), minimize = c(TRUE, TRUE))
#' addOptPathEl(op, x = list(x = 1), y = c(5, 3))
#' addOptPathEl(op, x = list(x = 2), y = c(2, 4))
#' addOptPathEl(op, x = list(x = 3), y = c(9, 4))
#' addOptPathEl(op, x = list(x = 4), y = c(4, 9))
#' as.data.frame(op)
#' getOptPathParetoFront(op)
#' getOptPathParetoFront(op, index = TRUE)
getOptPathParetoFront = function(op, y.names = op$y.names, dob = op$env$dob, eol = op$env$eol, index = FALSE) {
  assertClass(op, "OptPath")
  assertCharacter(y.names, len = 2L)
  assertSubset(y.names, op$y.names, empty.ok = FALSE)
  dob = asInteger(dob, any.missing = TRUE)
  eol = asInteger(eol, any.missing = TRUE)
  assertFlag(index, na.ok = TRUE)
  requirePackages("emoa")
  life.inds = which(op$env$dob %in% dob & op$env$eol %in% eol)
  if (length(life.inds) == 0)
    stop("No element found which matches dob and eol restrictions!")
  y = getOptPathY(op, y.names, drop = FALSE)[life.inds, ]
  # multiply columns with -1 if maximize
  k = ifelse(op$minimize, 1, -1)
  y2 = t(y) * k
  nondom = which(!is_dominated(y2))
  if (index)
    return(life.inds[nondom])
  else
    y[nondom,]
}

#' Get y-vector or y-matrix from the optimization path.
#'
#' @template arg_op
#' @param names [\code{character}]\cr
#'   Names of performance measure.
#'   Default is all performance measures in path.
#' @param drop [\code{logical(1)}]\cr
#'   Return vector instead of matrix when only one y-column was selected?
#'   Default is \code{TRUE}.
#' @return [\code{numeric} | \code{matrix}]. The columns of the matrix are always named.
#' @export
#' @family optpath
getOptPathY = function(op, names, drop = TRUE) {
  UseMethod("getOptPathY")
}

#' Get date-of-birth vector from the optimization path.
#'
#' @template arg_op
#' @return [\code{integer}].
#' @export
#' @family optpath
getOptPathDOB = function(op) {
  UseMethod("getOptPathDOB")
}

#' Get end-of-life vector from the optimization path.
#'
#' @template arg_op
#' @return [\code{integer}].
#' @export
#' @family optpath
getOptPathEOL = function(op) {
  UseMethod("getOptPathEOL")
}

#' Get error-message vector from the optimization path.
#'
#' @template arg_op
#' @return [\code{character}].
#' @export
#' @family optpath
getOptPathErrorMessages = function(op) {
  UseMethod("getOptPathErrorMessages")
}

#' Get exec-time vector from the optimization path.
#'
#' @template arg_op
#' @return [\code{numeric}].
#' @export
#' @family optpath
getOptPathExecTimes = function(op) {
  UseMethod("getOptPathExecTimes")
}

#' Get column from the optimization path.
#'
#' @template arg_op
#' @param name [\code{character(1)}]\cr
#'   Names of the column
#' @return [\code{data.frame}].
#' @export
#' @family optpath
getOptPathCol = function(op, name) {
  UseMethod("getOptPathCol")
}

#' Set the dates of birth of parameter values, in-place.
#'
#' @template arg_op
#' @param index [\code{integer}]\cr
#'   Vector of indices of elements.
#' @param dob [integer] \cr
#'   Dates of birth, single value or same length of \code{index}.
#' @return Nothing.
#' @export
#' @family optpath
setOptPathElDOB = function(op, index, dob) {
  assertClass(op, "OptPath")
  index = asInteger(index)
  dob = asInteger(dob)
  op$env$dob[index] = dob
  invisible(NULL)
}


#' Set the end of life dates of parameter values, in-place.
#'
#' @template arg_op
#' @param index [\code{integer}]\cr
#'   Vector of indices of elements.
#' @param eol [integer] \cr
#'   EOL dates, single value or same length of \code{index}.
#' @return Nothing.
#' @export
#' @family optpath
setOptPathElEOL = function(op, index, eol) {
  assertClass(op, "OptPath")
  index = asInteger(index)
  eol = asInteger(eol)
  op$env$eol[index] = eol
  invisible(NULL)
}
