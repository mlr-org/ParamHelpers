#' @title Create a description object for a parameter.
#'
#' @description
#' For each parameter type a special constructor function is available, see below.
#'
#' For the following arguments you can also pass an \code{expression} instead of a concrete value:
#' \code{default}, \code{len}, \code{lower}, \code{upper}, \code{values}.
#' These expressions can depend on arbitrary symbols, which are later filled in / substituted from
#' a dictionary, in order to produce a concrete valu, see \code{\link{evaluateParamExpressions}}.
#' So this enables data / context dependent settings, which is sometimes useful.
#'
#' The S3 class is a list which stores these elements:
#' \describe{
#'   \item{id [\code{character(1)}]}{See argument of same name.}
#'   \item{type [\code{character(1)}]}{Data type of parameter. For all type string see \code{\link{getTypeStringsAll}}}
#'   \item{len [\code{integer(1)} | \code{expression}]}{See argument of same name.}
#'   \item{lower [\code{numeric} | \code{expression}]}{See argument of same name. Length of this vector is \code{len}.}
#'   \item{upper [\code{numeric} | \code{expression}]}{See argument of same name. Length of this vector is \code{len}.}
#'   \item{values [\code{list} | \code{expression}]}{Discrete values, always stored as a named list.}
#'   \item{cnames [\code{character}}{See argument of same name.}
#'   \item{allow.inf [\code{logical(1)}]}{See argument of same name.}
#'   \item{trafo [\code{NULL} | \code{function(x)}]}{See argument of same name.}
#'   \item{requires [\code{NULL} | \code{expression}]}{See argument of same name.}
#'   \item{default [any concrete value | \code{expression}]}{See argument of same name.}
#'   \item{has.default [\code{logical(1)}]}{Extra flag to really be able to check whether the user passed a default, to avoid troubles with \code{NULL} and \code{NA}.}
#'   \item{tunable [\code{logical(1)}]}{See argument of same name.}
#'   \item{special.vals [\code{list}]}{See argument of same name.}
#' }
#'
#' @param id [\code{character(1)}]\cr
#'   Name of parameter.
 #' @param len [\code{integer(1)} | \code{expression}]\cr
#'   Length of vector parameter.
#' @param lower [\code{numeric} | \code{expression}]\cr
#'   Lower bounds.
#'   A singe value of length 1 is automatically replicated to \code{len} for vector parameters.
#'   If \code{len = NA} you can only pass length-1 scalars.
#'   Default is \code{-Inf}.
#' @param upper [\code{numeric} | \code{expression}]\cr
#'   Upper bounds.
#'   A singe value of length 1 is automatically replicated to \code{len} for vector parameters.
#'   If \code{len = NA} you can only pass length-1 scalars.
#'   Default is \code{Inf}.
#' @param values [\code{vector} | \code{list} | \code{expression}]\cr
#'   Possible discrete values. Instead of using a vector of atomic values,
#'   you are also allowed to pass a list of quite \dQuote{complex} R objects,
#'   which are used as discrete choices. If you do the latter,
#'   the elements must be uniquely named, so that the names can be used
#'   as internal representations for the choice.
#' @param cnames [\code{character}]\cr
#'   Component names for vector params (except discrete).
#'   Every function in this package that creates vector values for such a param, will name
#'   that vector with \code{cnames}.
#' @param allow.inf [\code{logical(1)}]\cr
#'   Allow infinite values for numeric and numericvector params to be feasible settings.
#'   Default is \code{FALSE}.
#' @param default [any concrete value | \code{expression}]\cr
#'   Default value used in learner.
#'   Note: When this is a discrete parameter make sure to use a VALUE here, not the NAME of the value.
#'   If this argument is missing, it means no default value is available.
#' @param trafo [\code{NULL} | \code{function(x)}]\cr
#'   Function to transform parameter. It should be applied to the parameter value
#'   before it is, e.g., passed to a corresponding objective function.
#'   Function must accept a parameter value as the first argument and return a transformed one.
#'   Default is \code{NULL} which means no transformation.
#' @param requires [\code{NULL} | \code{call} | \code{expression}]\cr
#'   States requirements on other parameters' values, so that setting
#'   this parameter only makes sense if its requirements are satisfied (dependent parameter).
#'   Can be an object created either with \code{expression} or \code{quote},
#'   the former type is auto-converted into the later.
#'   Only really useful if the parameter is included in a \code{\link{ParamSet}}.
#'   Note that if your dependent parameter is a logical Boolean you need to verbosely write
#'   \code{requires = quote(a == TRUE)} and not \code{requires = quote(a)}.
#'   Default is \code{NULL} which means no requirements.
#' @param tunable [\code{logical(1)}]\cr
#'   Is this parameter tunable?
#'   Defining a parameter to be not-tunable allows to mark arguments like, e.g., \dQuote{verbose} or
#'   other purely technical stuff, and allows them to be excluded from later automatic optimization
#'   procedures that would try to consider all available parameters.
#'   Default is \code{TRUE} (except for \code{untyped}, \code{function}, \code{character} and
#'   \code{characterVector}) which means it is tunable.
#' @param special.vals [\code{list()}]\cr
#'   A list of special values the parameter can except which are outside of the defined range.
#'   Default is an empty list.
#' @return [\code{\link{Param}}].
#' @name Param
#' @rdname Param
#' @examples
#' makeNumericParam("x",lower = -1, upper = 1)
#' makeNumericVectorParam("x", len = 2)
#' makeDiscreteParam("y", values = c("a","b"))
#' makeCharacterParam("z")
NULL

makeParam = function(id, type, learner.param, len = 1L, lower = NULL, upper = NULL, values = NULL, cnames = NULL, allow.inf = FALSE, default,
  trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), when) {
  assertString(id)
  assert(
    checkCount(len, na.ok = learner.param),
    checkClass(len, "expression")
  )
  if (isNumericTypeString(type, include.int = TRUE)) {
    assert(
      checkNumeric(lower, any.missing = FALSE),
      checkClass(lower, "expression")
    )
    assert(
      checkNumeric(upper, any.missing = FALSE),
      checkClass(upper, "expression")
    )
    # the following check also ensures that if len=NA, the lower and upper must be scalars
    if (!is.expression(len) && !is.expression(lower)) {
      if (length(lower) %nin% c(1L, len))
        stopf("For param '%s' length 'lower' must be either 1 or length of param, not: %i", id, length(lower))
    }
    if (!is.expression(len) && !is.expression(upper)) {
      if (length(upper) %nin% c(1L, len))
        stopf("For param '%s' length 'upper' must be either 1 or length of param, not: %i", id, length(upper))
    }
  }
  if (isDiscreteTypeString(type)) {
    values = checkValuesForDiscreteParam(id, values)
  }
  #We cannot check default} for NULL or NA as this could be the default value!
  if (missing(default)) {
    has.default = FALSE
    default = NULL
  } else {
    has.default = TRUE
  }
  # FIXME: Do we need to check for NA here? Hopefully not because this might occur in mlr?
  if (has.default && isScalarNA(default))
    warningf("NA used as a default value for learner parameter %s.\nParamHelpers uses NA as a special value for dependent parameters.", id)
  if (!is.null(trafo))
    assertFunction(trafo)
  if (!is.null(requires)) {
    requires = convertExpressionToCall(requires)
    assertClass(requires, "call")
  }
  assertList(special.vals)

  if (isNumericTypeString(type, include.int = TRUE)) {
    if (!is.expression(len) && !is.na(len) && len > 1L) {
      if (isScalarNumeric(lower))
        lower = rep(lower, len)
      if (isScalarNumeric(upper))
        upper = rep(upper, len)
    }
    if (!is.expression(lower) && !is.expression(upper)) {
     if (any(upper < lower))
        stopf("For param '%s' some component of 'upper' is smaller than the corresponding one in 'lower'", id)
    }
  }
  p = makeS3Obj("Param",
    id = id,
    type = type,
    len = len,
    lower = lower,
    upper = upper,
    values = values,
    cnames = cnames,
    allow.inf = allow.inf,
    has.default = has.default,
    default = default,
    trafo = trafo,
    requires = requires,
    tunable = tunable,
    special.vals = special.vals
  )
  if (learner.param)
    p = makeLearnerParam(p, when)
  if (has.default && !is.expression(default)) {
    if (!isFeasible(p, default))
      stop(p$id, " : 'default' must be a feasible parameter setting.")
  }
  return(p)
}

getParPrintData = function(x, trafo = TRUE, used = TRUE, constr.clip = 40L) {
  g = function(n) collapse(sprintf("%.3g", n))
  if (isNumeric(x, include.int = TRUE)) {
    if (!is.expression(x$lower) && !is.expression(x$upper) &&
      (length(unique(x$lower)) == 1L) && (length(unique(x$upper)) == 1L)) {
      x$lower = unique(x$lower)
      x$upper = unique(x$upper)
    }
    low = if (is.expression(x$lower))  as.character(x$lower) else g(x$lower)
    upp = if (is.expression(x$upper)) as.character(x$upper) else g(x$upper)
    constr = sprintf("%s to %s", low, upp)
  } else if (isDiscrete(x, include.logical = FALSE)) {
    vals = if (is.expression(x$values)) as.character(x$values) else collapse(names(x$values))
    constr = clipString(vals, constr.clip)
  } else
    constr = "-"
  if (x$has.default) {
    if (!is.expression(x$default)) {
      def = x$default
      def = paramValueToString(x, def)
    } else
      def = as.character(x$default)
  } else {
    def = "-"
  }
  if (isVector(x)) {
    if (!is.expression(x$len))
      len = x$len
    else
      len = as.character(x$len)
  } else {
    len = "-"
  }
  d = data.frame(
    Type = x$type,
    len = len,
    Def = def,
    Constr = constr,
    Req = ifelse(is.null(x$requires), "-", "Y"),
    Tunable = x$tunable,
    stringsAsFactors = FALSE
  )
  if (trafo)
    d$Trafo = ifelse(is.null(x$trafo), "-", "Y")
  return(d)
}

#' @export
print.Param = function(x, ..., trafo = TRUE) {
  print(getParPrintData(x, trafo = trafo))
}

# helper function to perform sanity checks on values of disctrete param
checkValuesForDiscreteParam = function(id, values) {
  if (is.vector(values) && !is.expression(values))
    values = as.list(values)
  assert(
    checkList(values),
    checkClass(values, "expression")
  )
  if (!is.expression(values)) {
    if (length(values) == 0L)
      stopf("No possible value for discrete parameter %s!", id)

    # check that NA does not occur in values, we use that for "missing state" for dependent params
    # make sure that this works for complex object too, cannot be done with simple is.na
    if (any(vlapply(values, isScalarNA)))
      stopf("NA is not allowed as a value for discrete parameter %s.\nParamHelpers uses NA as a special value for dependent parameters.", id)

    n = length(values)
    ns = names(values)
    # if names missing, set all to ""
    if (is.null(ns))
      ns = rep("", n)
    # guess missing names
    for (i in seq_len(n)) {
      v = values[[i]]
      if(is.na(ns[i]) || ns[i] == "") {
        if (is.character(v) || is.numeric(v))
          ns[i] = as.character(v)
      }
    }
    names(values) = ns
    if (!isProperlyNamed(values)) {
      stopf("Not all values for parameter '%s' were named and names could not be guessed!", id)
    }

    # check that NA does not occur in value names, see above
    if ("NA" %in% names(values))
      stopf("NA is not allowed as a value name for discrete parameter %s.\nParamHelpers uses NA as a special value for dependent parameters.", id)
  }
  return(values)
}

