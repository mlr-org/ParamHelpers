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

  ### recode types to integers
  types = extractSubList(par.set$pars, "type")
  lens = getParamLengths(par.set)
  lookup = rep(1:4, each=2L)
  nlookup = c("numeric", "numericvector", "integer", "integervector",
              "discrete", "discretevector", "logical", "logicalvector")
  int.type = lookup[match(types, nlookup)]
  int.type[is.na(int.type)] = 99L
  int.type = rep.int(int.type, lens)


  ### fix type conversions ... rather ugly
  df = convertDataFrameCols(df, factors.as.char=TRUE)
  ints.as.double = mapply(function(type, col) type == 2L && is.double(col), type=int.type, col=df)
  df[ints.as.double] = lapply(df[ints.as.double], as.integer)
  logicals.as.char = mapply(function(type, col) type == 4L && is.character(col), type=int.type, col=df)
  df[logicals.as.char] = lapply(df[logicals.as.char], as.logical)

  .Call("c_dfRowsToList", df, par.set$pars, int.type, names(par.set$pars), lens, PACKAGE="ParamHelpers")
}

#' @export
#' @rdname dfRowsToList
dfRowToList = function(df, par.set, i) {
  dfRowsToList(df, par.set)[[i]]
}
