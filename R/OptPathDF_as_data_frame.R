#FIXME: document with args
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
  return(res)
}



