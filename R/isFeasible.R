#' Check if parameter value is valid.
#'
#' Check if a parameter value satisfies the constraints of the
#' parameter description. This includes the \code{requires} expressions and
#' the \code{forbidden} expression, if \code{par} is a \code{\link{ParamSet}}.
#' If \code{requires} is not satisfied,
#' the parameter value must be set to scalar \code{NA} to be still feasible, a single scalar even in a
#' case of a vector parameter.
#'
#' @template arg_par_or_set
#' @param x [any] \cr
#'   Single value to check.
#'   For a parameter set this must be a list in the correct order.
#' @return logical(1)
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
isFeasible = function(par, x) {
  UseMethod("isFeasible")
}

#' @export
isFeasible.Param = function(par, x) {
  # we dont have to consider requires here, it is not a param set
  constraintsOkParam(par, x)
}

#' @export
isFeasible.LearnerParam = function(par, x) {
  # we dont have to consider requires here, it is not a param set
  constraintsOkLearnerParam(par, x)
}

#' @export
isFeasible.ParamSet = function(par, x) {
  if (!(is.list(x) && length(x) == length(par$pars) && all(names(x) == names(par$pars))))
    return(FALSE)
  if (isForbidden(par, x))
    return(FALSE)
  #FIXME: very slow
  for (i in seq_along(par$pars)) {
    p = par$pars[[i]]
    v = x[[i]]
    # no requires, just check constraints
    if(is.null(p$requires)) {
      if (!isFeasible(p, v))
        return(FALSE)
    } else {
      # requires, is it ok?
      if (!requiresOk(par, x, i)) {
        # if not, val must be NA
        if (!isScalarNA(v))
          return(FALSE)
      } else {
        # requires, ok, check constraints
        if (!isFeasible(p, v))
          return(FALSE)
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
  inValues = function(v) any(sapply(par$values, function(w) isTRUE(all.equal(w, v))))
  if (type == "numeric")
    is.numeric(x) && length(x) == 1 && is.finite(x) && x >= par$lower && x <= par$upper
  else if (type == "integer")
    is.numeric(x) && length(x) == 1 && is.finite(x) && x >= par$lower && x <= par$upper && x == as.integer(x)
  else if (type == "numericvector")
    is.numeric(x) && length(x) == par$len && all(is.finite(x) & x >= par$lower & x <= par$upper)
  else if (type == "integervector")
    is.numeric(x) && length(x) == par$len && all(is.finite(x) & x >= par$lower & x <= par$upper & x == as.integer(x))
  else if (type == "discrete")
    inValues(x)
  else if (type == "discretevector")
    is.list(x) && length(x) == par$len && all(sapply(x, inValues))
  else if (type == "logical")
    is.logical(x) && length(x) == 1 && !is.na(x)
  else if (type == "logicalvector")
    is.logical(x) && length(x) == par$len && !any(is.na(x))
  else if (type == "function")
    is.function(x)
}

constraintsOkLearnerParam = function(par, x) {
  inValues = function(v) any(sapply(par$values, function(w) isTRUE(all.equal(w, v))))
  type = par$type
  # extra case for unkown dim in vector
  if (type == "numericvector")
    is.numeric(x) && (is.na(par$len) || length(x) == par$len) && all(is.finite(x) & x >= par$lower & x <= par$upper)
  else if (type == "integervector")
    is.numeric(x) && (is.na(par$len) || length(x) == par$len) && all(is.finite(x) & x >= par$lower & x <= par$upper & x == as.integer(x))
  else if (type == "logicalvector")
    is.logical(x) && (is.na(par$len) || length(x) == par$len) && !any(is.na(x))
  else if (type == "discretevector")
    is.list(x) && (is.na(par$len) || length(x) == par$len) && all(sapply(x, inValues))
  else
    constraintsOkParam(par, x)
}

# is the requires part of the ith param valid for value x (x[[i]] is value or ith param)
# assumes that param actually has a requires part
requiresOk = function(par.set, x, i) {
  eval(par.set$pars[[i]]$requires, envir = x)
}
