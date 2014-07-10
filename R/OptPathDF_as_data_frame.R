#' Convert optimization path to data.frame.
#'
#' @param x [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @param row.names [\code{character}]\cr
#'   Row names for result.
#'   Default is none.
#' @param optional \cr
#'   Currently ignored.
#' @param discretes.as.factor [\code{logical(1)}]\cr
#'   Represent discrete param columns as factors (or characters)?
#'   Default is \code{FALSE}.
#' @param include.x [\code{logical(1)}]\cr
#'   Include all input params?
#'   Default is \code{TRUE}.
#' @param include.y [\code{logical(1)}]\cr
#'   Include all y-columns?
#'   Default is \code{TRUE}.
#' @param include.rest [\code{logical(1)}]\cr
#'   Include all other columns?
#'   Default is \code{TRUE}.
#' @return [\code{data.frame}].
#' @export
as.data.frame.OptPathDF = function(x, row.names = NULL, optional = FALSE,
  discretes.as.factor = FALSE, include.x = TRUE, include.y = TRUE, include.rest = TRUE, ...) {

  assertFlag(include.x)
  assertFlag(include.y)
  assertFlag(include.rest)

  if (!include.x && !include.y && !include.rest)
    stopf("Not able to create data.frame from opt.path. You need to include something!")

  res = makeDataFrame(nrow = getOptPathLength(x), ncol = 0)

  if (include.x || include.y) {
    df = x$env$path
    y.cols = which(colnames(df) %in% x$y.names)
    if (include.x)
      res = cbind(res, df[, -y.cols, drop = FALSE])
    if (include.y)
      res = cbind(res, df[, y.cols, drop = FALSE])
    res = convertDataFrameCols(res, chars.as.factor = discretes.as.factor)
  }
  if (include.rest) {
    res = cbind(res, dob = x$env$dob, eol = x$env$eol)
    # if err message / exec time included, add it
    if (!is.null(x$env$error.message))
      res$error.message = x$env$error.message
    if (!is.null(x$env$exec.time))
      res$exec.time = x$env$exec.time
    if (!is.null(x$env$extra))
      res = cbind(res, convertListOfRowsToDataFrame(x$env$extra))
  }
  if (!is.null(row.names)) {
    assertCharacter(row.names, len = nrow(res), any.missing = FALSE)
    rownames(res) = row.names
  }
  return(res)
}



