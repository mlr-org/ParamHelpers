#' @title Construct a parameter set.
#'
#' @description
#' `makeParamSet`: Construct from a bunch of parameters.
#'
#' Multiple sets can be concatenated with `c`.
#'
#' The constructed S3 class is simply a list that contains the element `pars`.
#' `pars` is a list of the passed parameters, named by their ids.
#'
#' If `keys` are provided it will automatically be checked whether all
#' expressions within the provided parameters only contain arguments that are a
#' subset of keys.
#'
#' @param ... ([Param()])\cr
#'   Parameters.
#' @param params (list of [Param()])\cr
#'   List of parameters, alternative way instead of using `...`.
#' @param forbidden (`NULL` | R expression)\cr
#'   States forbidden region of parameter set via an expression. Every setting
#'   which satisfies this expression is considered to be infeasible. This makes
#'   it possible to exclude more complex region of the parameter space than
#'   through simple constraints or `requires`-conditions (although these should
#'   be always used when possible). If parameters have associated trafos, the
#'   forbidden region must always be specified on the original scale and not the
#'   transformed one. Default is `NULL` which means no forbidden region.
#' @template arg_keys
#' @return [ParamSet()] | `LearnerParamSet`.
#'   If all parameters of the `ParamSet` are learner parameters, the output
#'   will inherit the class `LearnerParamSet`.
#' @aliases ParamSet
#' @export
#' @examples
#' makeParamSet(
#'   makeNumericParam("u", lower = 1),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeLogicalParam("x"),
#'   makeDiscreteVectorParam("y", len = 2, values = c("a", "b"))
#' )
#' makeParamSet(
#'   makeNumericParam("u", lower = expression(ceiling(n))),
#'   makeIntegerParam("v", lower = expression(floor(n)), upper = 2),
#'   keys = c("p", "n")
#' )
#' makeParamSet(
#'   makeNumericParam("min", lower = 0, upper = 0.8),
#'   makeNumericParam("max", lower = 0.2, upper = 1),
#'   forbidden = expression(min > max)
#' )
makeParamSet = function(..., params = NULL, forbidden = NULL, keys = NULL) {

  pars = list(...)
  if (length(pars) > 0 && !is.null(params)) {
    stop("You can only use one of ... or params!")
  }
  if (!is.null(params)) {
    assertList(params, types = "Param")
    pars = params
  } else {
    assertList(pars, types = "Param")
  }
  ns = extractSubList(pars, "id")
  if (anyDuplicated(ns)) {
    stop("All parameters must have unique names!")
  }
  names(pars) = ns
  par.set = makeS3Obj("ParamSet", pars = pars, forbidden = forbidden)

  if (length(pars) > 0L) {
    # if all Params are LearnerParams, then the ParSet is considered
    # to be a LearnerParSet and we automatically extend the keys by
    # the default keys from mlr, i.e. task, n, p, k and type
    if (all(vlapply(pars, inherits, what = "LearnerParam"))) {
      par.set = addClasses(par.set, classes = "LearnerParamSet")
      keys = union(keys, c("task", "n", "p", "k", "type"))
    }
    if (!is.null(keys) && (hasExpression(par.set))) {
      checkExpressionFeasibility(par.set = par.set, keys = keys)
    }
  }
  return(par.set)
}

getParSetPrintData = function(x, trafo = TRUE, used = TRUE, constr.clip = 40L) {
  d = lapply(x$pars, getParPrintData, trafo = trafo, used = used, constr.clip = constr.clip)
  return(do.call(rbind, d))
}

#' @export
print.ParamSet = function(x, ..., trafo = TRUE, used = TRUE, constr.clip = 40L) {
  if (isEmpty(x)) {
    print("Empty parameter set.")
  } else {
    print(getParSetPrintData(x, trafo = trafo, used = used, constr.clip = constr.clip))
  }
  if (hasForbidden(x)) {
    catf("Forbidden region specified.")
  }
  return(invisible(NULL))
}

#' @export
c.ParamSet = function(..., recursive = FALSE) {
  pss = list(...)
  pars = Reduce(c, lapply(pss, function(ps) ps$pars))
  # remove the names here. if 'params' is a par name, this wont work in the
  # contructor call but we are allowed to pass the list without names, as they
  # are set again automatically later for pars
  names(pars) = NULL
  return(do.call(makeParamSet, pars))
}

#' Check whether parameter set is empty.
#'
#' @param par.set (ParamSet()])\cr
#'   Parameter set.
#' @return `logical(1)`.
#' @export
isEmpty = function(par.set) {
  assertClass(par.set, "ParamSet")
  UseMethod("isEmpty")
}

#' @export
isEmpty.ParamSet = function(par.set) {
  return(length(par.set$pars) == 0)
}

#' `makeNumericParamSet`: Convenience function for numerics.
#'
#' @param id (`character(1)`)\cr
#'   Name of parameter.
#' @param len (`integer(1)`)\cr
#'   Length of vector.
#' @param lower (`numeric`)\cr
#'   Lower bound.
#'   Default is `-Inf`.
#' @param upper [numeric] \cr
#'   Upper bound.
#'   Default is `Inf`.
#' @param vector (`logical(1)`) \cr
#'   Should a `NumericVectorParam` be used instead of
#'   n `NumericParam` objects?
#'   Default is `TRUE`.
#' @rdname makeParamSet
#' @export
makeNumericParamSet = function(id = "x", len, lower = -Inf, upper = Inf, vector = TRUE) {
  assertString(id)
  if (missing(len)) {
    if (!missing(lower)) {
      len = length(lower)
    } else if (!missing(upper)) {
      len = length(upper)
    }
  } else {
    len = asInt(len)
  }
  if (is.numeric(lower) && length(lower) == 1L) {
    lower = rep(lower, len)
  }
  if (is.numeric(upper) && length(upper) == 1L) {
    upper = rep(upper, len)
  }
  assertNumeric(lower, len = len)
  assertNumeric(upper, len = len)
  assertFlag(vector)
  if (vector) {
    return(makeParamSet(makeNumericVectorParam(id = id, len = len, lower = lower, upper = upper)))
  } else {
    return(makeParamSet(params = lapply(1:len, function(i) {
      makeNumericParam(id = paste(id, i, sep = ""), lower = lower[i], upper = upper[i])
    })))
  }
}
