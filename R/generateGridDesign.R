#' @title Generates a grid design for a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  \code{numeric}  \cr
#'  integer(vector)   \tab  \code{integer}  \cr
#'  discrete(vector)  \tab  \code{factor} (names of values = levels) \cr
#'  logical(vector)   \tab  \code{logical}
#' }
#' If you want to convert these, look at \code{\link[BBmisc]{convertDataFrameCols}}.
#' Dependent parameters whose constraints are unsatisfied generate \code{NA} entries in their
#' respective columns.
#' For discrete vectors the levels and their order will be preserved.
#'
#' The algorithm currently performs these steps:
#' \enumerate{
#'   \item{We create a grid. For numerics and integers we use the specfied resolution. For discretes all values will be taken.}
#'   \item{Forbidden points are removed.}
#'   \item{Parameters are trafoed (potentially, depending on the setting of argument \code{trafo});
#'   dependent parameters whose constraints are unsatisfied are set to \code{NA} entries.}
#'   \item{Duplicated points are removed. Duplicated points are not generated in a
#'    grid design, but the way parameter dependencies are handled make this possible.}
#' }
#'
#' Note that if you have trafos attached to your params, the complete creation of the design
#' (except for the detection of invalid parameters w.r.t to their \code{requires} setting)
#' takes place on the UNTRANSFORMED scale. So this function creates a regular grid
#' over the param space on the UNTRANSFORMED scale, but not necessarily the transformed scale.
#'
#' \code{generateDesign} will NOT work if there are dependencies over multiple levels of
#' parameters and the dependency is only given with respect to the \dQuote{previous} parameter.
#' A current workaround is to state all dependencies on all parameters involved.
#' (We are working on it.)
#'
#' @template arg_parset
#' @param resolution [\code{integer}]\cr
#'   Resolution of the grid for each numeric/integer parameter in \code{par.set}.
#'   For vector parameters, it is the resolution per dimension.
#'   Either pass one resolution for all parameters, or a named vector.
#' @template arg_trafo
#' @template ret_gendes_df
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("x1", lower = -2, upper = 1),
#'   makeNumericParam("x2", lower = -2, upper = 2, trafo = function(x) x^2)
#' )
#' generateGridDesign(ps, resolution = c(x1 = 4, x2 = 5), trafo = TRUE)
generateGridDesign = function(par.set, resolution, trafo = FALSE) {
  doBasicGenDesignChecks(par.set)

  pars = par.set$pars
  n = length(pars)
  lens = getParamLengths(par.set)
  m = sum(lens)
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  par.set.num = filterParamsNumeric(par.set, include.int = TRUE)
  pids.num = getParamIds(par.set.num)

  if (hasNumeric(par.set, include.int = TRUE)) {
    if (isScalarNumeric(resolution)) {
      resolution = setNames(rep(resolution, length(pids.num)), pids.num)
    }
    resolution = asInteger(resolution, lower = 1L, len = length(pids.num))
    assertNamed(resolution, type = "named")
    if (!all(names(resolution) %in% pids.num))
      stop("'resolution' must be named with parameter ids!")
  }

  assertFlag(trafo)

  vals.list = setNames(vector("list", m), pids)
  el.counter = 1L

  # iterate over all params and discretize them
  for (i in seq_len(n)) {
    p = pars[[i]]
    if (isNumeric(p)) {
      lower = p$lower
      upper = p$upper
    }
    if (isDiscrete(p, include.logical = FALSE)) {
      discvals = p$values
    }

    # iterate over vector elements and d
    for (j in seq_len(p$len)) {
      if (isDiscrete(p, include.logical = FALSE)) {
        newvals = names(discvals)
      } else if (isLogical(p)) {
        newvals = c(TRUE, FALSE)
      } else if (isNumeric(p, include.int = TRUE)) {
        newvals = seq(from = lower[[j]], to = upper[[j]], length.out = resolution[[p$id]])
        # round for integer
        if (isInteger(p)) {
          newvals = as.integer(unique(round(newvals)))
        }
      } else {
        stopf("generateGridDesign cannot be used for param '%s' of type '%s'!", p$id, p$type)
      }
      vals.list[[el.counter]] = newvals
      el.counter = el.counter + 1
    }
  }
  res = expand.grid(vals.list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  # data types here:
  # num(vec): numeric
  # int(vec): integer
  # log(vec): logical
  # dis(vec): character

  colnames(res) = pids

  # check each row if forbidden, then remove
  if (hasForbidden(par.set)) {
    #FIXME: this is pretty slow, but correct
    fb = rowSapply(res, isForbidden, par.set = par.set)
    res = res[!fb, , drop = FALSE]
  }

  if (trafo || hasRequires(par.set)) {
    # the following lines are mainly copy paste from generateDesign
    types.df = getParamTypes(par.set, df.cols = TRUE)
    types.int = convertTypesToCInts(types.df)
    # ignore trafos if the user did not request transformed values
    trafos = if(trafo)
      lapply(pars, function(p) p$trafo)
    else
      replicate(length(pars), NULL, simplify = FALSE)
    par.requires = lapply(pars, function(p) p$requires)
    res = convertDataFrameCols(res, factors.as.char = TRUE)
    res = .Call(c_trafo_and_set_dep_to_na, res, types.int, names(pars), lens, trafos, par.requires, new.env())
  }

  # remove duplicates
  res = res[!duplicated(res), , drop = FALSE]

  res = convertDataFrameCols(res, chars.as.factor = TRUE)

  #fix factors
  res = fixDesignFactors(res, par.set)

  attr(res, "trafo") = trafo
  return(res)
}
