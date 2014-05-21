#FIXME: generateDesign will NOT work if there are dependencies
# over multiple levels of params and one only states the dependency only
#  wrt to the "last" param. also see daniels unit test.
#  it works as long all dependencies are stated, we need to at least document this


#' @title Generates a statistical design for a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  \code{numeric}  \cr
#'  integer(vector)   \tab  \code{integer}  \cr
#'  discrete(vector)  \tab  \code{factor}   \cr
#'  logical(vector)   \tab  \code{logical}
#' }
#' If you want to convert these, look at \code{\link[BBmisc]{convertDataFrameCols}}.
#'
#' Currently only lhs designs are supported.
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of samples in design.
#'   Default is 10.
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param fun [\code{function}]\cr
#'   Function from package lhs.
#'   Possible are: \code{\link[lhs]{maximinLHS}}, \code{\link[lhs]{randomLHS}},
#'   \code{\link[lhs]{geneticLHS}}, \code{\link[lhs]{improvedLHS}}, \code{\link[lhs]{optAugmentLHS}},
#'   \code{\link[lhs]{optimumLHS}}
#'   Default is \code{\link[lhs]{randomLHS}}.
#' @param fun.args [\code{list}]\cr
#'   List of further arguments passed to \code{fun}.
#' @param trafo [\code{logical(1)}]\cr
#'   Transform all parameters by using theirs respective transformation functions.
#'   Default is \code{FALSE}.
#' @param remove.duplicates [\code{logical(1)}]\cr
#'   Must the design NOT contain duplicate lines?
#'   Default is \code{FALSE}.
#' @param remove.duplicates.iter [\code{integer(1)}]\cr
#'   If \code{remove.duplicates} is set to \code{TRUE} and duplicates occur within the design, i. e., if
#'   at least one line appears multiple times, the function tries hard to fix to replace the duplicated lines.
#'   The parameter controls how many attempts the algorithm will at most start.
#' @return The created design is a data.frame. Columns are named by the ids of the parameters.
#'   If the \code{par.set} argument contains a vector parameter, its corresponding column names
#'   in the design are the parameter id concatenated with 1 to dimension of the vector.
#'   The data type of a column
#'   is defined in the following way. Numeric parameters generate numeric columns, integer parameters generate numeric/integer columns,
#'   logical parameters generate logical/factor columns.
#'   For discrete parameters the value names are used and character or factor columns are generated.
#'   Dependent parameters whose constaints are unsatisfied generate \code{NA} entries in their
#'   respective columns.
#'   The result will have an \code{logical(1)} attribute \dQuote{trafo},
#'   which is set to the value of argument \code{trafo}.
#' @export
#' @useDynLib ParamHelpers c_generateDesign1 c_generateDesign2
#' @examples
#' ps <- makeParamSet(
#'   makeNumericParam("x1", lower = -2, upper = 1),
#'   makeIntegerParam("x2", lower = 10, upper = 20)
#' )
#' # random latin hypercube design with 5 samples:
#' generateDesign(5, ps)
#'
#' # with trafo
#' ps <- makeParamSet(
#'   makeNumericParam("x", lower = -2, upper = 1),
#'   makeNumericVectorParam("y", len = 2, lower = 0, upper = 1, trafo = function(x) x/sum(x))
#' )
#' generateDesign(10, ps, trafo = TRUE)
generateDesign = function(n = 10L, par.set, fun, fun.args = list(), trafo = FALSE,
  remove.duplicates = FALSE, remove.duplicates.iter = 5L) {

  n = convertInteger(n)
  checkArg(n, "integer", len = 1L, na.ok = FALSE)
  z = doBasicGenDesignChecks(par.set)
  lower = z$lower; upper = z$upper

  requirePackages("lhs", "generateDesign")
  if (missing(fun))
    fun = lhs::randomLHS
  else
    checkArg(fun, "function")
  checkArg(fun.args, "list")
  checkArg(trafo, "logical", len = 1L, na.ok = FALSE)
  checkArg(remove.duplicates, "logical", len = 1L, na.ok = FALSE)
  checkArg(remove.duplicates.iter, "integer", len = 1L, lower = 1L, na.ok = FALSE)

  # recompute some useful stuff
  pars = par.set$pars
  lens = getParamLengths(par.set)
  k = sum(lens)
  pids1 = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  pids2 = getParamIds(par.set, repeated = TRUE, with.nr = FALSE)
  lower2 = setNames(rep(NA_real_, k), pids1)
  lower2 = insert(lower2, lower)
  upper2 = setNames(rep(NA_real_, k), pids1)
  upper2 = insert(upper2, upper)
  values = getValues(par.set)
  nlevs = setNames(rep(NA_integer_, k), pids1)
  for (i in seq_len(k))
    nlevs[i] = length(values[[pids2[i]]])

  # FIXME: most of the code structure really sucks in the whole function
  # we should probably introduce more helper functions to deal with that

  # allocate result df
  types.df = getTypes(par.set, df.cols = TRUE)
  types.int = convertTypesToCInts(types.df)
  types.df[types.df == "factor"] = "integer"
  res = makeDataFrame(n, k, col.types = types.df)

  # ignore trafos if the user did not request transformed values
  trafos = if(trafo)
    lapply(pars, function(p) p$trafo)
  else
    replicate(length(pars), NULL, simplify = FALSE)
  par.requires = lapply(pars, function(p) p$requires)

  des = do.call(fun, insert(list(n = n, k = k), fun.args))
  res = .Call(c_generateDesign1, des, res, types.int, lower2, upper2, nlevs)

  #FIXME: maybe do this in C
  # convert discrete integer coding back to chars
  for (i in seq_col(res)) {
    if (types.int[i] == 3L) {
      res[, i] = as.character(factor(res[, i],
        levels = seq_len(nlevs[i]), labels = names(values[[pids2[[i]]]])))
    }
  }
  res = .Call(c_generateDesign2, res, types.int, names(pars), lens, trafos, par.requires, new.env())

  colnames(res) = pids1
  res = convertDataFrameCols(res, chars.as.factor = FALSE)
  # explicitly set levels of factors so we have all value names as levels
  # FIXME all of this sucks and is ugly as sin...
  for (i in seq_col(res)) {
    if (types.int[i] == 3L) {
      res[, i] = factor(res[, i], levels = names(values[[pids2[[i]]]]))
    }
  }
  attr(res, "trafo") = trafo
  return(res)
}
