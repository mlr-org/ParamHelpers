#FIXME: generateDesign will NOT work if there are dependencies
# over multiple levels of params and one only states the dependency only
#  wrt to the "last" param. also see daniels unit test.
#  it works as long all dependencies are stated, we need to at least document this


#' Generates a statistical design for a parameter set.
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
#' @param ints.as.num [\code{logical(1)}]\cr
#'   Should parameters of type \dQuote{integer} or \dQuote{integervector} generate numeric columns?
#'   Default is \code{FALSE}.
#' @param discretes.as.factor [\code{logical(1)}]\cr
#'   Should discrete parameters have columns of type \dQuote{factor} in result?
#'   Otherwise character columns are generated.
#'   Default is \code{TRUE}.
#' @param logicals.as.factor [\code{logical(1)}]\cr
#'   Should logical parameters have columns of type \dQuote{factor} in result?
#'   Otherwise logical columns are generated.
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
  ints.as.num = FALSE,  discretes.as.factor = TRUE, logicals.as.factor = FALSE,
  remove.duplicates = FALSE, remove.duplicates.iter = 5L) {

  n = convertInteger(n)
  checkArg(n, "integer", len = 1L, na.ok = FALSE)
  checkArg(par.set, "ParamSet")
  requirePackages("lhs", "generateDesign")
  if (missing(fun))
    fun = lhs::randomLHS
  else
    checkArg(fun, "function")
  checkArg(fun.args, "list")
  checkArg(trafo, "logical", len = 1L, na.ok = FALSE)
  checkArg(ints.as.num, "logical", len = 1L, na.ok = FALSE)
  checkArg(discretes.as.factor, "logical", len = 1L, na.ok = FALSE)
  checkArg(remove.duplicates, "logical", len = 1L, na.ok = FALSE)
  checkArg(remove.duplicates.iter, "integer", len = 1L, lower = 1L, na.ok = FALSE)

  if (isEmpty(par.set))
    stop("par.set must not be empty!")
  if(any(sapply(par.set$pars, function(x) inherits(x, "LearnerParameter"))))
    stop("No par.set parameter in 'generateDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
  lower = getLower(par.set, with.nr = TRUE)
  upper = getUpper(par.set, with.nr = TRUE)
  values = getValues(par.set)

  if (any(is.infinite(c(lower, upper))))
    stop("generateDesign requires finite box constraints!")

  pars = par.set$pars
  pids1 = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  pids2 = getParamIds(par.set, repeated = TRUE, with.nr = FALSE)
  lens = getParamLengths(par.set)
  k = sum(lens)

  # FIXME: most of the code structure really sucks in the whole function
  # we should probably introduce more helper functions to deal with that

  # allocate result df
  types.df = getTypes(par.set, df.cols = TRUE)
  types.int = convertTypesToCInts(types.df)
  types.df[types.df == "factor"] = "integer"
  res = makeDataFrame(n, k, col.types = types.df)

  lower2 = setNames(rep(NA_real_, k), pids1)
  lower2 = insert(lower2, lower)
  upper2 = setNames(rep(NA_real_, k), pids1)
  upper2 = insert(upper2, upper)
  nlevs = setNames(rep(NA_integer_, k), pids1)
  for (i in seq_len(k))
    nlevs[i] = length(values[[pids2[i]]])
  # ignore trafos if the user did not request transformed values
  trafos = if(trafo)
    lapply(pars, function(p) p$trafo)
  else
    replicate(length(pars), NULL, simplify = FALSE)
  par.requires = lapply(pars, function(p) p$requires)

  des = do.call(fun, insert(list(n = n, k = k), fun.args))
  res = .Call(c_generateDesign1, des, res, types.int, lower2, upper2, nlevs)

  # try to replace duplicates a couple of times
  #FIXME: this stuff sucks as the whole function dows. We should definitely
  # reorganize the code!
  if (remove.duplicates) {
    to.remove = duplicated(res)
    to.keep = !to.remove
    nmissing = sum(to.remove)
    i = 0
    while(nmissing > 0 && i < remove.duplicates.iter) {
      des = des[to.keep,]
      des = do.call(lhs::augmentLHS, list(lhs = des, m = nmissing))
      res = .Call(c_generateDesign1, des, res, types.int, lower2, upper2, nlevs)
      to.remove = duplicated(res)
      to.keep = !to.remove
      nmissing = sum(to.remove)
      i = i + 1
    }
    if (nmissing > 0) {
      stopf("Algorithm was unable to generate design with no duplicated rows!")
    }
  }

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
  res = convertDataFrameCols(res, ints.as.num = ints.as.num,
    chars.as.factor = FALSE, logicals.as.factor = logicals.as.factor)
  # explicitly set levels of factors so we have all value names as levels
  # FIXME all of this sucks and is ugly as sin...
  if (discretes.as.factor) {
    for (i in seq_col(res)) {
      if (types.int[i] == 3L) {
        res[, i] = factor(res[, i], levels = names(values[[pids2[[i]]]]))
      }
    }
  }
  attr(res, "trafo") = trafo
  return(res)
}
