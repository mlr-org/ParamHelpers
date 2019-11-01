#' Returns type information for a parameter set.
#'
#' @template arg_parset
#' @param df.cols (`logical(1)`)\cr
#'   If `FALSE` simply return the parameter types in the set,
#'   i.e., `par$type`.
#'   If `TRUE`, convert types so they correspond to R types of a data.frame
#'   where values of this set might be stored.
#'   This also results in replication of output types for
#'   vector parameters.
#'   Default is `FALSE`.
#' @param df.discretes.as.factor (`logical(1)`)\cr
#'   If `df.cols` is `TRUE`:
#'   Should type for discrete params be `factor` or `character`?
#'   Default is `TRUE`.
#' @param use.names (`logical(1)`)\cr
#'   Name the result vector?
#'   Default is `FALSE`.
#' @param with.nr (`logical(1)`)\cr
#'   Should number from 1 to length be appended to name?
#'   Only used if `use.names` and `df.cols` are `TRUE`.
#'   Default is `TRUE`.
#' @return [`character`].
#' @export
getParamTypes = function(par.set, df.cols = FALSE, df.discretes.as.factor = TRUE,
  use.names = FALSE, with.nr = TRUE) {

  assertClass(par.set, "ParamSet")
  assertFlag(df.cols)
  assertFlag(df.discretes.as.factor)
  assertFlag(use.names)
  assertFlag(with.nr)

  types = extractSubList(par.set$pars, "type")
  if (length(types) == 0L) {
    return(character(0L))
  }

  recode = function(types, from, to) {
    i = fmatch(types, from, nomatch = 0L)
    types[i > 0L] = to[i]
    rep(types, getParamLengths(par.set))
  }


  if (df.cols) {
    to = if (df.discretes.as.factor) {
      c("numeric", "integer", "factor", "factor", "logical", "character")
    } else {
      c("numeric", "integer", "character", "character", "logical", "character")
    }
    types = recode(types, ph$convert.param.types.from, to)
  }

  ns = if (use.names) {
    getParamIds(par.set, repeated = df.cols, with.nr = with.nr)
  } else {
    NULL
  }
  return(setNames(types, ns))
}
