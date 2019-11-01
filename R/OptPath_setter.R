#' Set the dates of birth of parameter values, in-place.
#'
#' @template arg_op
#' @param index [integer]\cr
#'   Vector of indices of elements.
#' @param dob [integer] \cr
#'   Dates of birth, single value or same length of `index`.
#' @return Nothing.
#' @export
#' @family optpath
setOptPathElDOB = function(op, index, dob) {
  assertClass(op, "OptPath")
  index = asInteger(index)
  dob = asInteger(dob)
  op$env$dob[index] = dob
  return(invisible(NULL))
}


#' Set the end of life dates of parameter values, in-place.
#'
#' @template arg_op
#' @param index [integer]\cr
#'   Vector of indices of elements.
#' @param eol [integer] \cr
#'   EOL dates, single value or same length of `index`.
#' @return Nothing.
#' @export
#' @family optpath
setOptPathElEOL = function(op, index, eol) {
  assertClass(op, "OptPath")
  index = asInteger(index)
  eol = asInteger(eol)
  op$env$eol[index] = eol
  return(invisible(NULL))
}

#' @title Add a new element to an optimization path.
#'
#' @description
#' Changes the argument in-place. Note that when adding parameters that have
#' associated transformations, it is probably best to add the untransformed
#' values to the path. Otherwise you have to switch off the feasibility check,
#' as constraints might now not hold anymore.
#'
#' Dependent parameters whose requirements are not satisfied must be represented
#' by a scalar NA in the input.
#'
#' @template arg_op
#' @param x (`list`)\cr
#'   List of parameter values for a point in input space. Must be in same order
#'   as parameters.
#' @param y (`numeric`)\cr
#'   Vector of fitness values.  Must be in same order as `y.names`.
#' @param dob (`integer(1)`)\cr
#'   Date of birth of the new parameters.
#'   Default is length of path + 1.
#' @param eol (`integer(1)`)\cr
#'   End of life of point.
#'   Default is `NA`.
#' @param error.message (`character(1)`)\cr
#'   Possible error message that occurred for this parameter values.
#'   Default is `NA`.
#' @param exec.time (`numeric(1)`)\cr
#'   Possible exec time for this evaluation.
#'   Default is `NA`.
#' @param extra (`list`)\cr
#'   Possible list of extra values to store.
#'   The list must be fully named. The list can contain nonscalar values, but
#'   these nonscalar entries must have a name starting with a dot (`.`).
#'   Other entries must be scalar, and must be in the same order of all calls of
#'   `addOptPathEl`.
#'   Watch out: if `include.extra` was set to `TRUE` in (makeOptPathDF())
#'   the list of extras is mandatory.
#'   Default is `NULL`.
#' @param check.feasible (`logical(1)`)\cr
#'   Should `x` be checked with (isFeasible())?
#'   Default is `TRUE`.
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
addOptPathEl = function(op, x, y, dob = getOptPathLength(op) + 1L, eol = as.integer(NA),
  error.message = NA_character_, exec.time = NA_real_, extra = NULL,
  check.feasible = !op$add.transformed.x) {
  UseMethod("addOptPathEl")
}
