#' Create optimization path.
#'
#' Optimizers can iteratively log their evaluated points
#' into this object. Can be converted into a data.frame with
#' \code{as.data.frame(x, discrete.as.factor = TRUE / FALSE)}.
#'
#' A optimization path has a number of path elements, where each element consists of: the value of the
#' decision variables at this point, the values of the performance measures at this point, the date-of-birth (dob)
#' of this point and the end-of-life (eol) of this point.
#'
#' For discrete parameters always the name of the value is stored as a character.
#' When you retrieve an element with \code{\link{getOptPathEl}}, this name is converted to the actual discrete value.
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
#' \item{env [\code{environment}]}{Environment which stores the optimization path. Contents depend on implementation.}
#' }
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set for the decision variables that are optimized.
#' @param y.names [\code{character}]\cr
#'   Names of performance measures that are optimized or logged.
#' @param minimize [\code{logical}]\cr
#'   Which of the performance measures in y.names should be minimized?
#'   Vector of booleans in the same order as \code{y.names}.
#' @param add.transformed.x [\code{logical(1)}]\cr
#'   If some parameters have associated transformations, are you going to
#'   add x values after they have been transformed?
#'   Default is \code{FALSE}.
#' @name OptPath
#' @rdname OptPath
#' @seealso \code{\link{getOptPathLength}}, \code{\link{getOptPathEl}}, \code{\link{addOptPathEl}}, \code{\link{getOptPathY}}, \code{\link{setOptPathElDOB}}, \code{\link{setOptPathElEOL}}
NULL

makeOptPath = function(par.set, y.names, minimize, add.transformed.x=FALSE) {
  ok = c("numeric", "integer", "numericvector", "integervector", "logical", "logicalvector", "discrete", "discretevector")
  if(length(par.set$pars) > length(filterParams(par.set, ok)$pars))
    stop("OptPath can currently only be used for: ", paste(ok, collapse=","))
  x.names = getParamIds(par.set)
  # be really sure that x and y columns are uniquely named
  x.names2 = c(getParamIds(par.set, with.nr=TRUE), getParamIds(par.set, with.nr=FALSE))
  if (length(intersect(x.names2, y.names)) > 0)
    stop("'x.names' and 'y.names' must not contain common elements!")
  if (length(minimize) != length(y.names))
    stop("'y.names' and 'minimize' must be of the same length!")
  if (is.character(names(minimize)) && !setequal(names(minimize), y.names))
    stop("Given names for 'minimize' must be the same as 'y.names'!")
  if (is.null(names(minimize)))
    names(minimize) = y.names
  if (any(c(" ", "eol") %in% (union(x.names, y.names))))
    stop("'dob' and 'eol' are not allowed in parameter names or 'y.names'!")
  structure(list(
    par.set = par.set,
    y.names = y.names,
    minimize = minimize,
    add.transformed.x = add.transformed.x,
    env = new.env()
  ), class="OptPath")
}

#' @S3method print OptPath
print.OptPath = function(x, ...) {
  catf("Optimization path")
  catf("  Dimensions: x=%i/%i, y=%i",
    length(x$par.set$pars), sum(getParamLengths(x$par.set)), length(x$y.names))
  catf("  Length: %i", getOptPathLength(x))
  catf("  Add x values transformed: %s", x$add.transformed.x)
}


#' Get the length of the optimization path.
#'
#' Dependent parameters whose requirements are not satisfied are represented by a scalar NA in the output.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @return [\code{integer(1)}]
#' @export
getOptPathLength = function(op) {
  UseMethod("getOptPathLength")
}

#' Get an element from the optimization path.
#'
#' Dependent parameters whose requirements 
#' are not satisfied are represented by a scalar NA in the elements of \code{x} 
#' of the return value.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param index [\code{integer(1)}]\cr
#'   Index of element.
#' @return List with elements \code{x} [named \code{list}], \code{y} [named \code{numeric}],
#'   \code{dob} [\code{integer(1)}] and \code{eol} [\code{integer(1)}].
#' @rdname getOptPathEl
#' @export
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
#' Dependent parameters whose requirements are not satisfied must be represented by a scalar NA in the input.
#' 
#' @param op [\code{\link{OptPath}}] \cr
#'   Optimization path.
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
#' @param check.feasible [\code{logical(1)}]\cr
#'   Should \code{x} be checked with \code{\link{isFeasible}}?
#'   Default is \code{TRUE}.
#' @return Nothing.
#' @export
#' @examples
#' ps <- makeParamSet(
#'   makeNumericParam("p1"),
#'   makeDiscreteParam("p2", values=c("a", "b"))
#' )
#' op <- makeOptPathDF(par.set=ps, y.names="y", minimize=TRUE)
#' addOptPathEl(op, x=list(p1=7, p2="b"), y=1)
#' addOptPathEl(op, x=list(p1=-1, p2="a"), y=2)
#' as.data.frame(op)
addOptPathEl = function(op, x, y, dob=getOptPathLength(op)+1L, eol=as.integer(NA), check.feasible=!op$add.transformed.x) {
  UseMethod("addOptPathEl")
}

#' Get index of the best element from optimization path.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
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
#' @examples
#' ps <- makeParamSet(makeNumericParam("x"))
#' op <- makeOptPathDF(par.set=ps, y.names="y", minimize=TRUE)
#' addOptPathEl(op, x=list(x=1), y=5)
#' addOptPathEl(op, x=list(x=2), y=3)
#' addOptPathEl(op, x=list(x=3), y=9)
#' addOptPathEl(op, x=list(x=4), y=3)
#' as.data.frame(op)
#' getOptPathBestIndex(op)
#' getOptPathBestIndex(op, ties="first")
getOptPathBestIndex = function(op, y.name=op$y.names[1], dob=op$env$dob, eol=op$env$eol, ties="last") {
  checkArg(ties, choices=c("all", "first", "last", "random"))
  life.inds = which(op$env$dob %in% dob & op$env$eol %in% eol)
  if (length(life.inds) == 0)
    stop("No element found which matches dob and eol restrictions!")
  y = getOptPathY(op, y.name)[life.inds]
  if (all(is.na(y))) {
    best.inds = life.inds
  } else {
    if (op$minimize[y.name])
      best.inds = which(min(y, na.rm=TRUE) == y)
    else
      best.inds = which(max(y, na.rm=TRUE) == y)
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

#' Get y vector from the optimization path.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param name [\code{character(1)}]\cr
#'   Name of performance measure.
#' @return [\code{numeric}].
#' @export
getOptPathY = function(op, name) {
  UseMethod("getOptPathY")
}

#' Get date-of-birth vector from the optimization path.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @return [\code{integer}].
#' @export
getOptPathDOB = function(op) {
  UseMethod("getOptPathDOB")
}

#' Get end-of-life vector from the optimization path.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @return [\code{integer}].
#' @export
getOptPathEOL = function(op) {
  UseMethod("getOptPathEOL")
}

#' Set the dates of birth of parameter values, in-place.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param index [\code{integer}]\cr
#'   Vector of indices of elements.
#' @param dob [integer] \cr
#'   Dates of birth, single value or same length of \code{index}.
#' @return Nothing.
#' @export
setOptPathElDOB = function(op, index, dob) {
  checkArg(op, "OptPath")
  index = convertIntegers(index)
  checkArg(index, "integer", na.ok=FALSE)
  dob = convertIntegers(dob)
  checkArg(dob, "integer")
  op$env$dob[index] = dob
  invisible(NULL)
}


#' Set the end of life dates of parameter values, in-place.
#'
#' @param op [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param index [\code{integer}]\cr
#'   Vector of indices of elements.
#' @param eol [integer] \cr
#'   EOL dates, single value or same length of \code{index}.
#' @return Nothing.
#' @export
setOptPathElEOL = function(op, index, eol) {
  checkArg(op, "OptPath")
  index = convertIntegers(index)
  checkArg(index, "integer", na.ok=FALSE)
  eol = convertIntegers(eol)
  checkArg(eol, "integer")
  op$env$eol[index] = eol
  invisible(NULL)
}

