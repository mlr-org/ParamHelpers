
#' Convert a data.frame row to list of parameter-value-lists.
#' 
#' Dependent parameters whose requirements are not satisfied are represented by a scalar NA in the output.
#'
#' @param df [\code{data.frame}]\cr
#'   Data.frame, potentially from \code{\link{OptPathDF}}.
#'   Columns are assumed to be in the same order as par.set. 
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param i [\code{integer(1)}]\cr
#'   Row index.
#' @return [\code{list}]. Named by parameter ids. 
#' @export
#' @useDynLib ParamHelpers c_dfRowsToList
#' @rdname dfRowsToList
dfRowsToList = function(df, par.set) {
  checkArg(df, "data.frame")
  checkArg(par.set, "ParamSet")

  lens = getParamLengths(par.set)

  types2 = as.integer(unlist(sapply(par.set$pars, function(x) {
    y = x$type
    y =if (y %in% c("numeric", "numericvector"))
      1L
    else if (y %in% c("integer", "integervector"))
      2L
    else if (y %in% c("discrete", "discretevector"))
      3L
    else if (y %in% c("logical", "logicalvector"))
      4L
    else 99L
    rep(y, x$len)
  })))
  df = convertDataFrameCols(df, factors.as.char=TRUE)
  # if we have ints encoded as numerics, recode them
  for (j in seq_col(df)) {
    if (types2[j] == 2L && is.double(df[,j]))
      df[,j] = as.integer(df[,j])
  }
  .Call(c_dfRowsToList, df, par.set$pars, types2, names(par.set$pars), lens)
}

#' @export
#' @rdname dfRowsToList
dfRowToList = function(df, par.set, i) {
  dfRowsToList(df, par.set)[[i]]
}
