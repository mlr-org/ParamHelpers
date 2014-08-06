#' @title Convert a data.frame row to list of parameter-value-lists.
#'
#' @description
#' Please note that (naturally) the columns of \code{df} have to be of the correct type
#' type w.r.t. the corresponding parameter. The only exception are integer parameters
#' where the corresponding columns in \code{df} are allowed to be numerics.
#'
#' \tabular{ll}{
#'  numeric(vector)   \tab  \code{numeric}  \cr
#'  integer(vector)   \tab  \code{integer}  \cr
#'  discrete(vector)  \tab  \code{factor} (names of values = levels) \cr
#'  logical(vector)   \tab  \code{logical}
#' }
#'
#' Dependent parameters whose requirements are not satisfied are represented by a scalar
#' NA in the output.
#'
#' @param df [\code{data.frame}]\cr
#'   Data.frame, potentially from \code{\link{OptPathDF}}.
#'   Columns are assumed to be in the same order as par.set.
#' @template arg_parset
#' @param i [\code{integer(1)}]\cr
#'   Row index.
#' @return [\code{list}]. Named by parameter ids.
#' @export
#' @useDynLib ParamHelpers c_dfRowsToList
#' @rdname dfRowsToList
dfRowsToList = function(df, par.set) {
  assertClass(df, "data.frame")
  assertClass(par.set, "ParamSet")

  lens = getParamLengths(par.set)
  cnames = extractSubList(par.set$pars, "cnames", simplify = FALSE)
  int.type = convertTypesToCInts(getParamTypes(par.set, df.cols = TRUE))

  # factors to chars, so we can evaluate requires
  df = convertDataFrameCols(df, factors.as.char = TRUE)
  # ints might just be encoded as nums in df, convert before going to C
  ints.as.double = mapply(function(type, col) type == 2L && is.double(col), type = int.type, col = df)
  df[ints.as.double] = lapply(df[ints.as.double], as.integer)

  .Call("c_dfRowsToList", df, par.set$pars, int.type, names(par.set$pars), lens, cnames, PACKAGE = "ParamHelpers")
}

#' @export
#' @rdname dfRowsToList
dfRowToList = function(df, par.set, i) {
  dfRowsToList(df, par.set)[[i]]
}
