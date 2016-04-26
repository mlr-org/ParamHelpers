#' Convert optimization path to data.frame.

#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  x-numeric(vector)   \tab  \code{numeric}  \cr
#'  x-integer(vector)   \tab  \code{integer}  \cr
#'  x-discrete(vector)  \tab  \code{factor} (names of values = levels) \cr
#'  x-logical(vector)   \tab  \code{logical} \cr
#'  y-columns           \tab  \code{numeric}  \cr
#'  dob                 \tab  \code{integer}  \cr
#'  eol                 \tab  \code{integer}  \cr
#'  error.message       \tab  \code{character} \cr
#'  exec.time           \tab  \code{numeric}  \cr
#'  extra-columns       \tab  any \cr
#' }
#' If you want to convert these, look at \code{\link[BBmisc]{convertDataFrameCols}}.
#' Dependent parameters whose constraints are unsatisfied generate \code{NA} entries in their
#' respective columns.
#'
#' @param x [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param row.names [\code{character}]\cr
#'   Row names for result.
#'   Default is none.
#' @param include.x [\code{logical(1)}]\cr
#'   Include all input params?
#'   Default is \code{TRUE}.
#' @param include.y [\code{logical(1)}]\cr
#'   Include all y-columns?
#'   Default is \code{TRUE}.
#' @param include.rest [\code{logical(1)}]\cr
#'   Include all other columns?
#'   Default is \code{TRUE}.
#' @template arg_opgetter_dob
#' @template arg_opgetter_eol
#' @param ... [any] \cr
#'   Currently ignored.
#' @return [\code{data.frame}].
#' @export
as.data.frame.OptPathDT = function(x, row.names = NULL, include.x = TRUE, include.y = TRUE, include.rest = TRUE,
  dob = x$env$dob, eol = x$env$eol, ...) {

  assertFlag(include.x)
  assertFlag(include.y)
  assertFlag(include.rest)
  dob = asInteger(dob)
  eol = asInteger(eol)

  if (!include.x && !include.y && !include.rest)
    stopf("Not able to create data.frame from opt.path. You need to include something!")

  ind = getOptPathDobAndEolIndex(x, dob, eol)
  if (!any(ind))
    stopf("No elements where selected (via 'dob' and 'eol')!")

  sel.cols = c("dob", "eol")
  if (include.x)
    sel.cols = c(sel.cols, getParSetNamesForTable(x$par.set))
  if (include.y)
    sel.cols = c(sel.cols, x$y.names)
  if (include.rest)
    sel.cols = c(sel.cols, getOptPathExtraNames(x))

  res = res[ind, sel.cols, drop = FALSE, with = FALSE]
  if (!is.null(row.names)) {
    assertCharacter(row.names, len = nrow(res), any.missing = FALSE)
    rownames(res) = row.names
  }
  return(res)
}



