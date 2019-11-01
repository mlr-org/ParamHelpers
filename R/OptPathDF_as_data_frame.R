#' Convert optimization path to data.frame.

#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  x-numeric(vector)   \tab  `numeric`  \cr
#'  x-integer(vector)   \tab  `integer`  \cr
#'  x-discrete(vector)  \tab  `factor` (names of values = levels) \cr
#'  x-logical(vector)   \tab  `logical` \cr
#'  y-columns           \tab  `numeric`  \cr
#'  dob                 \tab  `integer`  \cr
#'  eol                 \tab  `integer`  \cr
#'  error.message       \tab  `character` \cr
#'  exec.time           \tab  `numeric`  \cr
#'  extra-columns       \tab  any \cr
#' }
#' If you want to convert these, look at [BBmisc::convertDataFrameCols()].
#' Dependent parameters whose constraints are unsatisfied generate `NA` entries
#' in their respective columns. Factor columns of discrete parameters always
#' have their complete level set from the `param.set`.
#'
#' @param x ([OptPath()])\cr
#'   Optimization path.
#' @param row.names [character]\cr
#'   Row names for result.
#'   Default is none.
#' @param optional (any)\cr
#'   Currently ignored.
#' @param include.x (`logical(1)`)\cr
#'   Include all input params?
#'   Default is `TRUE`.
#' @param include.y (`logical(1)`)\cr
#'   Include all y-columns?
#'   Default is `TRUE`.
#' @param include.rest (`logical(1)`)\cr
#'   Include all other columns?
#'   Default is `TRUE`.
#' @template arg_opgetter_dob
#' @template arg_opgetter_eol
#' @param ... (any) \cr
#'   Currently ignored.
#' @return [`data.frame`].
#' @export
as.data.frame.OptPathDF = function(x, row.names = NULL, optional = FALSE, include.x = TRUE, include.y = TRUE, include.rest = TRUE, dob = x$env$dob, eol = x$env$eol, ...) {

  assertFlag(include.x)
  assertFlag(include.y)
  assertFlag(include.rest)
  dob = asInteger(dob)
  eol = asInteger(eol)

  if (!include.x && !include.y && !include.rest) {
    stopf("Not able to create data.frame from opt.path. You need to include something!")
  }

  ind = getOptPathDobAndEolIndex(x, dob, eol)
  if (!any(ind)) {
    stopf("No elements where selected (via 'dob' and 'eol')!")
  }

  res = makeDataFrame(nrow = sum(ind), ncol = 0)

  if (include.x || include.y) {
    df = x$env$path[ind, , drop = FALSE]
    y.cols = which(colnames(df) %in% x$y.names)
    if (include.x) {
      x.df = df[, -y.cols, drop = FALSE]
      x.df = fixDesignFactors(x.df, x$par.set) # keeps factor levels
      res = cbind(res, x.df)
    }
    if (include.y) {
      res = cbind(res, df[, y.cols, drop = FALSE])
    }
    res = convertDataFrameCols(res, chars.as.factor = TRUE)
  }
  if (include.rest) {
    res = cbind(res, dob = x$env$dob[ind], eol = x$env$eol[ind])
    # if err message / exec time included, add it
    if (!is.null(x$env$error.message)) {
      res$error.message = x$env$error.message[ind]
    }
    if (!is.null(x$env$exec.time)) {
      res$exec.time = x$env$exec.time[ind]
    }
    if (!is.null(x$env$extra)) {
      extra.clean = lapply(x$env$extra[ind], removeDotEntries)
      res = cbind(res, convertListOfRowsToDataFrame(extra.clean))
    }
  }
  if (!is.null(row.names)) {
    assertCharacter(row.names, len = nrow(res), any.missing = FALSE)
    rownames(res) = row.names
  }
  return(res)
}

# remove all named entries that have a name starting with a dot
removeDotEntries = function(l) {
  l[!startsWith(names(l), ".")]
}
