# FIXME: generateDesign will NOT work if there are dependencies
# over multiple levels of params and one only states the dependency only
#  wrt to the "last" param. also see daniels unit test.
#  it works as long all dependencies are stated, we need to at least document this

# FIXME: it really makes no sense to calculate the distance for params that are NA
# when we do the design and augment it right? think about what happens here


#' @title Generates a statistical design for a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  `numeric`  \cr
#'  integer(vector)   \tab  `integer`  \cr
#'  discrete(vector)  \tab  `factor` (names of values = levels) \cr
#'  logical(vector)   \tab  `logical`
#' }
#' If you want to convert these, look at [BBmisc::convertDataFrameCols()].
#' Dependent parameters whose constraints are unsatisfied generate `NA` entries in their
#' respective columns.
#' For discrete vectors the levels and their order will be preserved, even if not all levels are present.
#'
#' Currently only lhs designs are supported.
#'
#' The algorithm currently iterates the following steps:
#' \enumerate{
#'   \item{We create a space filling design for all parameters, disregarding `requires`,
#'     a `trafo` or the forbidden region.}
#'   \item{Forbidden points are removed.}
#'   \item{Parameters are trafoed (potentially, depending on the setting of argument `trafo`);
#'     dependent parameters whose constraints are unsatisfied are set to `NA` entries.}
#'   \item{Duplicated design points are removed. Duplicated points are not generated in a
#'    reasonable space-filling design, but the way discrete parameters and also parameter dependencies
#'    are handled make this possible.}
#'   \item{If we removed some points, we now try to augment the design in a space-filling way
#'     and iterate.}
#' }
#'
#' Note that augmenting currently is somewhat experimental as we simply generate
#' missing points via new calls to [lhs::randomLHS()], but do not add points so
#' they are maximally far away from the already present ones. The reason is that
#' the latter is quite hard to achieve with complicated dependencies and
#' forbidden regions, if one wants to ensure that points actually get added...
#' But we are working on it.
#'
#' Note that if you have trafos attached to your params, the complete creation
#' of the design (except for the detection of invalid parameters w.r.t to their
#' `requires` setting) takes place on the UNTRANSFORMED scale. So this function
#' creates, e.g., a maximin LHS design on the UNTRANSFORMED scale, but not
#' necessarily the transformed scale.
#'
#' `generateDesign` will NOT work if there are dependencies over multiple levels
#' of parameters and the dependency is only given with respect to the
#' \dQuote{previous} parameter. A current workaround is to state all
#' dependencies on all parameters involved. (We are working on it.)
#'
#' @template arg_gendes_n
#' @template arg_parset
#' @param fun (`function`)\cr
#'   Function from package lhs.
#'   Possible are: [lhs::maximinLHS()], [lhs::randomLHS()],
#'   [lhs::geneticLHS()], [lhs::improvedLHS()], [lhs::optAugmentLHS()],
#'   [lhs::optimumLHS()]
#'   Default is [lhs::randomLHS()].
#' @param fun.args (`list`)\cr
#'   List of further arguments passed to `fun`.
#' @template arg_trafo
#' @param augment (`integer(1)`)\cr
#'   Duplicated values and forbidden regions in the parameter space can lead to
#'   the design becoming smaller than `n`. With this option it is possible to
#'   augment the design again to size `n`. It is not guaranteed that this always
#'   works (to full size) and `augment` specifies the number of tries to
#'   augment. If the the design is of size less than `n` after all tries, a
#'   warning is issued and the smaller design is returned. Default is 20.
#' @template ret_gendes_df
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("x1", lower = -2, upper = 1),
#'   makeIntegerParam("x2", lower = 10, upper = 20)
#' )
#' # random latin hypercube design with 5 samples:
#' generateDesign(5, ps)
#'
#' # with trafo
#' ps = makeParamSet(
#'   makeNumericParam("x", lower = -2, upper = 1),
#'   makeNumericVectorParam("y", len = 2, lower = 0, upper = 1, trafo = function(x) x / sum(x))
#' )
#' generateDesign(10, ps, trafo = TRUE)
generateDesign = function(n = 10L, par.set, fun, fun.args = list(), trafo = FALSE, augment = 20L) {

  n = asInt(n)
  z = doBasicGenDesignChecks(par.set)
  lower = z$lower
  upper = z$upper

  requirePackages("lhs", why = "generateDesign", default.method = "load")
  if (missing(fun)) {
    fun = lhs::randomLHS
  } else {
    assertFunction(fun)
  }
  assertList(fun.args)
  assertFlag(trafo)
  augment = asInt(augment, lower = 0L)

  ### precompute some useful stuff
  pars = par.set$pars
  lens = getParamLengths(par.set)
  k = sum(lens)
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  par.ids.each = lapply(pars, getParamIds, repeated = TRUE, with.nr = TRUE)
  par.nas.each = lapply(pars, getParamNA, repeated = FALSE)
  req.vectorized = determineReqVectorized(pars)
  lower2 = setNames(rep(NA_real_, k), pids)
  lower2 = insert(lower2, lower)
  upper2 = setNames(rep(NA_real_, k), pids)
  upper2 = insert(upper2, upper)
  values = getParamSetValues(par.set)
  types.df = getParamTypes(par.set, df.cols = TRUE)
  types.df[types.df == "factor"] = "character"

  nmissing = n
  # result objects
  res = data.frame()
  des = matrix(nrow = 0, ncol = k)
  for (iter in seq_len(augment)) {
    ### get design, types converted, trafos, conditionals set to NA
    # create new design or augment if we already have some points
    newdes = if (nmissing == n) {
      do.call(fun, insert(list(n = nmissing, k = k), fun.args))
    } else {
      lhs::randomLHS(nmissing, k = k)
    }

    # taken and adapted from individual Param Objects in mlr-org/paradox
    getMapping = function(i) {
      # Numeric
      if (types.df[i] == "numeric") {
        newdes[, i] * (upper2[i] - lower2[i]) + lower2[i]
      } else if (types.df[i] == "integer") {
        # Integer
        as.integer(floor(newdes[, i] * ((upper2[i] - lower2[i]) + 1L) * (1 - 1e-16)) + lower2[i])
        # Logic
      } else if (types.df[i] == "logical") {
        newdes[, i] < 0.5
        # Discrete
      } else if (types.df[i] == "character") {
        values[[i]][floor(newdes[, i] * length(values[[i]]) * (1 - 1e-16)) + 1]
      } else {
        stopf("%s for Param %s is an unsupported type.", types.df[i], pids[i])
      }
    }

    newres = mapDfc(seq_along(pids), getMapping)
    colnames(newres) = pids

    # check each row if forbidden, then remove
    if (hasForbidden(par.set)) {
      # FIXME: this is pretty slow, but correct
      fb = unlist(lapply(dfRowsToList(newres, par.set = par.set), function(x) {
        isForbidden(x, par.set = par.set)
      }))
      newres = newres[!fb, , drop = FALSE]
      newdes = newdes[!fb, , drop = FALSE]
    }
    if (trafo) {
      newres = applyTrafos(newres, pars)
    }

    newres = setRequiresToNA(newres, pars, par.ids.each, par.nas.each, req.vectorized)

    # add to result (design matrix and data.frame)
    des = rbind(des, newdes)
    res = rbind(res, newres)
    # remove duplicates
    to.remove = duplicated(res)
    des = des[!to.remove, , drop = FALSE]
    res = res[!to.remove, , drop = FALSE]
    nmissing = n - nrow(res)

    # Enough points? We are done!
    if (nmissing == 0L) {
      break
    }
  }

  if (nrow(res) < n) {
    warningf("generateDesign could only produce %i points instead of %i!", nrow(res), n)
  }

  colnames(res) = pids
  res = fixDesignFactors(res, par.set)
  attr(res, "trafo") = trafo
  return(res)
}

# applies the trafo to each parameter
# @param res data.frame()
#  with columns named accroding to getParamIds(par, repeated = TRUE, with.nr = TRUE) (so multiple columns for vector params)
# @pars list()
#  the ps$pars part of a param set
# @value data.frame()
applyTrafos = function(res, pars) {
  for (par in pars) {
    if (!is.null(par$trafo)) {
      ids = getParamIds(par, repeated = TRUE, with.nr = TRUE)
      if (par$len == 1) {
        # we expect, that the trafo works vectorized for normal params
        res[, ids] = par$trafo(res[, ids])
      } else {
        # for vector params the trafo has to work on the single vector
        for (i in seq_len(nrow(res))) {
          res[i, ids] = par$trafo(res[i, ids])
        }
      }
    }
  }
  res
}

# determines if the requirements work vectorized accrding to a simple heuristic
# @param pars list()
#   the ps$pars part of a param set
# @value logical named
#   TRUE for each column that I can evaluate vectorized
determineReqVectorized = function(pars) {
  # heuristic if we allow this requirement to be evaluated in an vectorized fashion
  vapply(X = lapply(pars, function(p) p$requires), function(req) {
    # vectorized if no "&&", "||" or "(" is detected
    !grepl(x = deparse(req), pattern = "\\|\\||&&|\\(")
  }, FUN.VALUE = logical(1))
}

# Sets values of params to NA if requirements are not evaluated to TRUE (rowwise)
# @param res data.frame(n,m)
#   The design
# @param pars list()
#   the ps$pars part of a param set
# @param pars.ids.each list()
#   the colnames that are used by each parameter (especially important for vector params, otherwise ist just list(paramA = "paramA"))
# @param pars.nas.each list()
#   the na type (e.g NA_character) that should be filled in if req is not met (important so that we do not destroy the right column type)
# @param req.vectorized named logical()
#   TRUE for each column that I can evaluate the req vectorized
# @value data.frame()
setRequiresToNA = function(res, pars, par.ids.each = NULL, par.nas.each = NULL, req.vectorized = NULL) {

  # these values can be passed manually to make this function faster if it is called multiple times because the single S3 function calls can sum up to some seconds!
  if (is.null(par.ids.each)) {
    par.ids.each = lapply(pars, getParamIds, repeated = TRUE, with.nr = TRUE)
  }
  if (is.null(par.nas.each)) {
    par.nas.each = lapply(pars, getParamNA, repeated = FALSE)
  }
  if (is.null(req.vectorized)) {
    req.vectorized = determineReqVectorized(pars)
  }

  for (par in pars) {
    req = par$requires
    if (!is.null(req)) {
      # set rows to NA 1) where req does not evalue to true AND 2) where the row is not already NA

      if (req.vectorized[par$id]) {
        set.to.na = !eval(req, res)
      } else {
        # unfortunately we allowed requirements to be not vectorized
        set.to.na = !vapply(seq_len(nrow(res)), function(i) {
          eval(req, res[i, ])
        }, FUN.VALUE = logical(1))
      }
      set.to.na = set.to.na & !is.na(res[[par.ids.each[[par$id]][1]]])
      res[set.to.na, par.ids.each[[par$id]]] = par.nas.each[[par$id]]
    }
  }
  res
}
