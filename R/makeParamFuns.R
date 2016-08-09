#' @rdname Param
#' @export
makeNumericParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE,
  default, trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list()) {

  ## Dirty hack! makeParam can not handle expressions, therefore we replace
  ## all expressions in lower, upper or defaults with feasible values prior
  ## to generating them and afterwards re-substitute them by the original
  ## expressions)
  lower.expr = upper.expr = default.expr = NULL
  if (is.expression(lower)) {
    lower.expr = lower
    lower = -Inf
  } else {
    assertNumber(lower)
  }
  if (is.expression(upper)) {
    upper.expr = upper
    upper = Inf
  } else {
    assertNumber(upper)
  }
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    if (is.infinite(lower) & is.infinite(upper))
      default = ifelse(lower == upper, lower, 0)
    else
      default = ifelse(is.finite(lower), lower, upper)
  }

  if (upper < lower)
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "numeric", len = 1L, lower = lower,
    upper = upper, allow.inf = allow.inf, values = NULL, cnames = NULL,
    default = default, trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for lower, upper and/or default
  ## (if they existed in the first place)
  if (!is.null(lower.expr))
    p$lower = lower.expr
  if (!is.null(upper.expr))
    p$upper = upper.expr
  if (!is.null(default.expr))
    p$default = default.expr
  return(p)
}

#' @rdname Param
#' @export
makeNumericVectorParam = function(id, len, lower = -Inf, upper = Inf,
  allow.inf = FALSE, cnames = NULL, default, trafo = NULL, requires = NULL,
  tunable = TRUE, special.vals = list()) {

  ## replace all expressions in length, lower, upper or defaults with feasible
  ## values prior to generating the parameter (and afterwards re-substitute them
  ## by the original expressions)
  len.expr = lower.expr = upper.expr = default.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len)
  }
  if (is.expression(lower)) {
    lower.expr = lower
    lower = -Inf
  } else {
    if (is.numeric(lower) && length(lower) == 1)
      lower = rep(lower, len)
      assertNumeric(lower, min.len = 1L, any.missing = FALSE)
  }
  if (is.expression(upper)) {
    upper.expr = upper
    upper = Inf
  } else {
    if (is.numeric(upper) && length(upper) == 1)
      upper = rep(upper, len)
    assertNumeric(upper, min.len = 1L, any.missing = FALSE)  
  }
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    if (any(is.infinite(lower) & is.infinite(upper)))
      default = ifelse(lower == upper, lower, 0)
    else
      default = ifelse(is.finite(lower), lower, upper)
    if (length(default) == 1)
    default = rep(default, len)
  }
  
  if (!is.null(cnames))
    assertCharacter(cnames, len = len, any.missing = FALSE)
  if (any(upper < lower))
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "numericvector", len = len, lower = lower,
    upper = upper, allow.inf = allow.inf, values = NULL, cnames = cnames,
    default = default, trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for lower, upper and/or default
  if (!is.null(lower.expr))
    p$lower = lower.expr
  if (!is.null(upper.expr))
    p$upper = upper.expr
  if (!is.null(default.expr))
    p$default = default.expr
  if (!is.null(len.expr))
    p$len = len.expr
  return(p)
}

#' @rdname Param
#' @export
makeIntegerParam = function(id, lower = -Inf, upper = Inf, default, trafo = NULL,
  requires = NULL, tunable = TRUE, special.vals = list()) {

  ## replace all expressions in lower, upper or defaults with feasible values
  ## prior to generating the parameter (and afterwards re-substitute them by the
  ## original expressions)
  lower.expr = upper.expr = default.expr = NULL
  if (is.expression(lower)) {
    lower.expr = lower
    lower = -Inf
  } else {
    assertNumber(lower)
  }
  if (is.expression(upper)) {
    upper.expr = upper
    upper = Inf
  } else {
    assertNumber(upper)
  }
  if (!missing(default) && is.expression(default)) {
    default.expr = default
  if (is.infinite(lower) & is.infinite(upper))
    default = ifelse(lower == upper, lower, 0)
  else
    default = ifelse(is.finite(lower), lower, upper)
  }

  if (upper < lower)
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "integer", len = 1L, lower = lower,
    upper = upper, values = NULL, cnames = NULL, default = default,
    trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for lower, upper and/or default
  if (!is.null(lower.expr))
    p$lower = lower.expr
  if (!is.null(upper.expr))
    p$upper = upper.expr
  if (!is.null(default.expr))
    p$default = default.expr
  return(p)
}

#' @rdname Param
#' @export
makeIntegerVectorParam = function(id, len, lower = -Inf, upper = Inf, cnames = NULL,
  default, trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list()) {

  ## replace all expressions in length, lower, upper or defaults with feasible
  ## values prior to generating the parameter (and afterwards re-substitute them
  ## by the original expressions)
  len.expr = lower.expr = upper.expr = default.expr = NULL
  upper = rep(upper, len)
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len)
  }
  if (is.expression(lower)) {
    lower.expr = lower
    lower = -Inf
  } else {
    if (is.numeric(lower) && length(lower) == 1)
      lower = rep(lower, len)
    assertNumeric(lower, min.len = 1L, any.missing = FALSE)
  }
  if (is.expression(upper)) {
    upper.expr = upper
    upper = Inf
  } else {
    if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, len)
    assertNumeric(upper, min.len = 1L, any.missing = FALSE)
  }
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    if (is.infinite(lower) & is.infinite(upper))
      default = ifelse(lower == upper, lower, 0)
    else
      default = ifelse(is.finite(lower), lower, upper)
    if (length(default) == 1)
      default = rep(default, len)
  }

  if (!is.null(cnames))
    assertCharacter(cnames, len = len, any.missing = FALSE)
  if (any(upper < lower))
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "integervector", len = len, lower = lower, upper = upper,
    values = NULL, cnames = cnames, default = default, trafo = trafo,
    requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for lower, upper and/or default
  if (!is.null(lower.expr))
    p$lower = lower.expr
  if (!is.null(upper.expr))
    p$upper = upper.expr
  if (!is.null(default.expr))
    p$default = default.expr
  if (!is.null(len.expr))
    p$len = len.expr
  return(p)
}

#' @rdname Param
#' @export
makeLogicalParam = function(id, default, requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  assertLogical(tunable, len = 1L)

  ## replace the expression in default with feasible values prior to generating
  ## the parameters (and then re-substituting them by the original expression)
  default.expr = NULL
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    default = TRUE
  }

  p = makeParam(id = id, type = "logical", len = 1L, lower = NULL,
    upper = NULL, values = values, cnames = NULL, default = default,
    trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for default
  if (!is.null(default.expr))
    p$default = default.expr
  return(p)
}

#' @rdname Param
#' @export
makeLogicalVectorParam = function(id, len, cnames = NULL, default,
  requires = NULL, tunable = TRUE, special.vals = list()) {

  ## replace expressions in length by feasible value prior to
  ## param construction (and replace it afterwards)
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len)
  }

  if (!is.null(cnames))
    assertCharacter(cnames, len = len, any.missing = FALSE)
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  assertLogical(tunable, len = 1L)

  ## replace the expression in default with feasible values prior to generating
  ## the parameters (and then re-substituting them by the original expression)
  default.expr = NULL
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    default = rep(TRUE, len)
  }

  p = makeParam(id = id, type = "logicalvector", len = len, lower = NULL,
    upper = NULL, values = values, cnames = cnames, default = default,
    trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for default
  if (!is.null(default.expr))
    p$default = default.expr
  if (!is.null(len.expr))
    p$len = len.expr
  return(p)
}

#' @rdname Param
#' @export
makeDiscreteParam = function(id, values, trafo = NULL, default,
  requires = NULL, tunable = TRUE, special.vals = list()) {

  ## replace the expression in values and defaults with feasible values prior to
  ## generating the params (and re-substituting them by the original expression)
  default.expr = values.expr = NULL
  if (!missing(values) && is.expression(values)) {
    values.expr = values
    values = c("a", "b")
  } else {
    values = checkValuesForDiscreteParam(id, values)
  }
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    default = values[[1]]
  }

  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "discrete", len = 1L, lower = NULL,
    upper = NULL, values = values, cnames = NULL, default = default,
    trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expression for default and values
  if (!is.null(default.expr))
    p$default = default.expr
  if (!is.null(values.expr))
    p$values = values.expr
  return(p)
}

#' @rdname Param
#' @export
makeDiscreteVectorParam = function(id, len, values, default, requires = NULL,
  tunable = TRUE, special.vals = list()) {

  ## replace the expressions in length with a feasible (1L) prior to generating
  ## the parameter (and afterwards re-substitute it by the original expression)
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len)
  }
  ## replace the expression in values and defaults with feasible values prior to
  ## generating the params (and re-substituting them by the original expression)
  default.expr = values.expr = NULL
  if (!missing(values) && is.expression(values)) {
    values.expr = values
    values = c("a", "b")
  } else {
    values = checkValuesForDiscreteParam(id, values)
  }
  if (!missing(default) && is.expression(default)) {
    default.expr = default
    default = rep(values[[1]], len)
  }

  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "discretevector", len = len, lower = NULL,
    upper = NULL, values = values, cnames = NULL, default = default,
    trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)

  ## re-substitute the expressions for default, values and length
  ## (if they existed in the first place)
  if (!is.null(default.expr))
    p$default = default.expr
  if (!is.null(values.expr))
    p$values = values.expr
  if (!is.null(len.expr))
    p$len = len.expr
  return(p)
}

#' @rdname Param
#' @export
makeFunctionParam = function(id, default = default, requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "function", len = 1L, lower = NULL, upper = NULL,
    values = NULL, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = FALSE, special.vals = special.vals)
}

#FIXME: what happens if NA is later used for untyped params? because we might interpret this as
# missing value wrt. dependent params
#' @rdname Param
#' @export
makeUntypedParam = function(id, default, requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "untyped", len = 1L, lower = NULL, upper = NULL,
    values = NULL, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = TRUE, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeCharacterParam = function(id, default, requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "character", len = 1L, lower = NULL, upper = NULL,
    values = NULL, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = FALSE, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeCharacterVectorParam = function(id, len, cnames = NULL, default,
  requires = NULL, special.vals = list()) {

  ## replace the expressions in length with a feasible (1L) prior to generating
  ## the parameter (and afterwards re-substitute it by the original expression)
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len)
  }

  p = makeParam(id = id, type = "charactervector", len = len, lower = NULL,
    upper = NULL, values = NULL, cnames = cnames, default = default,
    trafo = NULL, requires = requires, tunable = FALSE, special.vals = special.vals)

  ## re-substitute the length-expression (if it existed in the first place)
  if (!is.null(len.expr))
    p$len = len.expr
  return(p)
}

##### small helpers #####

checkValuesForDiscreteParam = function(id, values) {
  if (is.vector(values))
    values = as.list(values)
  assertList(values)

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

  return(values)
}
