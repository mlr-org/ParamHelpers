#' Returns type information for a parameter set.
#'
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param df.cols [\code{logical(1)}]\cr
#'   If \code{FALSE} simply return the parameter types in the set,
#'   i.e., \code{par$type}.
#'   If \code{TRUE}, convert types so they correspond to R types of a data.frame
#'   where values of this set might be stored.
#'   This also results in replication of output types for
#'   vector parameters.
#'   Default is \code{FALSE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Name the result vector?
#'   Default is \code{FALSE}.
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to name?
#'   Only used if \code{use.name} and are \code{TRUE}.
#'   Default is \code{TRUE}.
#' @return [\code{character}].
#' @export
getTypes = function(par.set, df.cols = FALSE, use.names = FALSE, with.nr = TRUE) {
  checkArg(par.set, "ParamSet")
  checkArg(df.cols, "logical", len = 1L, na.ok = FALSE)
  checkArg(use.names, "logical", len = 1L, na.ok = FALSE)
  checkArg(with.nr, "logical", len = 1L, na.ok = FALSE)

  types = extractSubList(par.set$pars, "type")
  recode = function(types, ...) {
    args = as.character(list(...))
    for (i in seq(1, length(args), 2)) {
      types[types == args[i]] = args[i+1]
    }
    types = rep(types, getParamLengths(par.set))
    return(types)
  }

  if (df.cols) {
    types = recode(types,
      "numericvector", "numeric",
      "integervector", "integer",
      "discrete", "factor",
      "discretevector", "factor",
      "logicalvector", "logical"
    )
   }

  ns = if (use.names)
    getParamIds(par.set, repeated = df.cols, with.nr = with.nr)
  else
    NULL
  setNames(types, ns)
}
