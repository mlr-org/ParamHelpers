#' @title Check if parameter value is valid.
#'
#' @description
#' Check if a parameter value satisfies the constraints of the parameter description.
#' This includes the \code{requires} expressions and the \code{forbidden} expression, if \code{par} is a \code{\link{ParamSet}}.
#' If \code{requires} is not satisfied, the parameter value must be set to scalar \code{NA} to be still feasible, a single scalar even in a case of a vector parameter.
#' If the result is \code{FALSE} the attribute \code{"warning"} is attached which gives the reason for the negative result.
#'
#' If the parameter has \code{cnames}, these are also checked.
#'
#' @template arg_par_or_set
#' @param x [any] \cr
#'   Single value to check.
#'   For a parameter set this must be a list.
#'   If the list is named, it is possible to only pass a subset of parameters defined
#'   in the \code{\link{ParamSet}} \code{par}. In that case, only conditions regarding the passed
#'   parameters are checked.
#'   (Note that this might not work if one of the passed params has a \code{requires} setting
#'   which refers to an unpassed param.)
#' @param use.defaults [\code{logical(1)}]\cr
#'   Whether defaults of the \code{\link{Param}}/\code{\link{ParamSet}} should be used if no values are supplied.
#'   Default is \code{FALSE}.
#' @param filter [\code{logical(1)}]\cr
#'   Whether the \code{\link{ParamSet}} should be reduced to the space of the given Param Values.
#'   Note that in case of \code{use.defaults = TRUE} the filtering will be conducted after the insertion of the default values.
#'   Default is \code{FALSE}.
#' @return [\code{logical(1)}].
#' @examples
#' p = makeNumericParam("x", lower = -1, upper = 1)
#' isFeasible(p, 0) # True
#' isFeasible(p, 2) # False, out of bounds
#' isFeasible(p, "a") # False, wrong type
#' # now for parameter sets
#' ps = makeParamSet(
#'   makeNumericParam("x", lower = -1, upper = 1),
#'   makeDiscreteParam("y", values = c("a", "b"))
#' )
#' isFeasible(ps, list(0, "a")) # True
#' isFeasible(ps, list("a", 0)) # False, wrong order
#' @export
isFeasible = function(par, x, use.defaults = FALSE, filter = FALSE) {
  UseMethod("isFeasible")
}

#' @export
isFeasible.Param = function(par, x, use.defaults = FALSE, filter = FALSE) {
  # we don't have to consider requires here, it is not a param set
  constraintsOkParam(par, x)
}

#' @export
isFeasible.LearnerParam = function(par, x, use.defaults = FALSE, filter = FALSE) {
  # we don't have to consider requires here, it is not a param set
  constraintsOkLearnerParam(par, x)
}

#' @export
isFeasible.ParamSet = function(par, x, use.defaults = FALSE, filter = FALSE) {
  named = testNamed(x)
  assertList(x)
  res = FALSE
  # insert defaults if they comply with the requirements
  if (named && use.defaults) {
    x = updateParVals(old.par.vals = getDefaults(par), new.par.vals = x, par.set = par)
  }
  if (!named && filter) {
    stopf("filter = TRUE only works with named input")
  }
  if (named && any(names(x) %nin% getParamIds(par)))
    stopf("Following names of given values do not match with ParamSet: %s", collapse(setdiff(names(x), getParamIds(par))))
  if (isForbidden(par, x)) {
    attr(res, "warning") = "The given parameter setting has forbidden values."
    return(res)
  }
  if (filter) {
    par = filterParams(par, ids = names(x))
    x = x[getParamIds(par)]
  } else if (length(x) != length(par$pars)) {
    stopf("Param setting of length %i does not match ParamSet length %i", length(x), length(par$pars))
  }
  if (!named) {
    names(x) = getParamIds(par)
  }
  missing.reqs = setdiff(getRequiredParamNames(par), names(x))
  if (length(missing.reqs) > 0)
    stopf("Following parameters are missing but needed for requirements: %s", collapse(missing.reqs))

  #FIXME: very slow
  for (i in seq_along(par$pars)) {
    p = par$pars[[i]]
    v = x[[i]]
    # no requires, just check constraints
    if (!requiresOk(p, x)) {
      # if not, val must be NA
      if (!isScalarNA(v)) {
        attr(res, "warning") = sprintf("Param %s is set but does not meet requirements %s", convertToShortString(x[i]), sQuote(collapse(deparse(p$requires), sep = "")))
        return(res)
      }
    } else {
      # requires, ok, check constraints
      if (!isFeasible(p, v)) {
        attr(res, "warning") = sprintf("The parameter setting %s does not meet constraints", convertToShortString(x[i]))
        return(res)
      }
    }
  }
  return(TRUE)
}

# are the contraints ok for value of a param (not considering requires)
constraintsOkParam = function(par, x) {
  type = par$type
  # this should work in any! case.
  if (type == "untyped")
    return(TRUE)
  inValues = function(v) any(vlapply(par$values, function(w) isTRUE(all.equal(w, v))))
  ok = if (type == "numeric")
    is.numeric(x) && length(x) == 1 && (par$allow.inf || is.finite(x)) && x >= par$lower && x <= par$upper
  else if (type == "integer")
    is.numeric(x) && length(x) == 1 && is.finite(x) && x >= par$lower && x <= par$upper && x == as.integer(x)
  else if (type == "numericvector")
    is.numeric(x) && length(x) == par$len && all((par$allow.inf | is.finite(x)) & x >= par$lower & x <= par$upper)
  else if (type == "integervector")
    is.numeric(x) && length(x) == par$len && all(is.finite(x) & x >= par$lower & x <= par$upper & x == as.integer(x))
  else if (type == "discrete")
    inValues(x)
  else if (type == "discretevector")
    is.list(x) && length(x) == par$len && all(vlapply(x, inValues))
  else if (type == "logical")
    is.logical(x) && length(x) == 1 && !is.na(x)
  else if (type == "logicalvector")
    is.logical(x) && length(x) == par$len && !anyMissing(x)
  else if (type == "character")
    is.character(x) && length(x) == 1 && !is.na(x)
  else if (type == "charactervector")
    is.character(x) && length(x) == par$len && !anyMissing(x)
  else if (type == "function")
    is.function(x)

  # if we have cnames, check them
  if (!is.null(par$cnames))
    ok = ok && !is.null(names(x)) && all(names(x) == par$cnames)

  return(ok)
}

constraintsOkLearnerParam = function(par, x) {
  inValues = function(v) any(vlapply(par$values, function(w) isTRUE(all.equal(w, v))))
  type = par$type
  # extra case for unkown dim in vector
  if (type == "numericvector")
    is.numeric(x) && (is.na(par$len) || length(x) == par$len) && all(is.finite(x) & x >= par$lower & x <= par$upper)
  else if (type == "integervector")
    is.numeric(x) && (is.na(par$len) || length(x) == par$len) && all(is.finite(x) & x >= par$lower & x <= par$upper & x == as.integer(x))
  else if (type == "logicalvector")
    is.logical(x) && (is.na(par$len) || length(x) == par$len) && !anyMissing(x)
  else if (type == "discretevector")
    is.list(x) && (is.na(par$len) || length(x) == par$len) && all(vlapply(x, inValues))
  else
    constraintsOkParam(par, x)
}

# checks if the requires part of the i-th param is valid for value x (x[[i]] is value or i-th param)
requiresOk = function(par, x) {
  if (is.null(par$requires)) {
    TRUE
  } else {
    isTRUE(eval(par$requires, envir = x))
  }
}
