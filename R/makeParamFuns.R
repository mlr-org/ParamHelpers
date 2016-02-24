#' @rdname Param
#' @param envir [\code{list}]\cr
#'   Environment, which will be used as "dictionary" to look whether the
#'   arguments of an expression (e.g. within the bounds, defaults, etc.) are
#'   feasible. The default is \code{NULL}.
#' @export
makeNumericParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE, default, trafo = NULL,
  requires = NULL, tunable = TRUE, envir = NULL) {

  assertString(id)
  # replace all expressions in lower, upper or defaults with feasible values
  # prior to generating the parameter (and afterwards re-substitute them by the
  # original expressions)
  lower.expr = upper.expr = default.expr = NULL
  if (is.expression(lower)) {
    checkExpressionFeasibility(id, lower, envir)
    lower.expr = lower
    lower = -Inf
  } else {
    assertNumber(lower)
  }
  if (is.expression(upper)) {
    checkExpressionFeasibility(id, upper, envir)
    upper.expr = upper
    upper = Inf
  } else {
    assertNumber(upper)
  }
  if (!missing(default) && is.expression(default)) {
    checkExpressionFeasibility(id, default, envir)
    default.expr = default
    if (is.infinite(lower) & is.infinite(upper))
      default = ifelse(lower == upper, lower, 0)
    else
      default = ifelse(is.finite(lower), lower, upper)
  }
  if (!is.null(trafo))
    assertFunction(trafo)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  if (upper < lower)
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "numeric", len = 1L, lower = lower, upper = upper, allow.inf = allow.inf,
    values = NULL, cnames = NULL, default = default, trafo = trafo,
    requires = requires, tunable = tunable)

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
makeNumericVectorParam = function(id, len, lower = -Inf, upper = Inf, allow.inf = FALSE, cnames = NULL,
  default, trafo = NULL, requires = NULL, tunable = TRUE, envir = NULL) {

  assertString(id)
  len = asInt(len)
  # replace all expressions in lower, upper or defaults with feasible values
  # prior to generating the parameter (and afterwards re-substitute them by the
  # original expressions)
  lower.expr = upper.expr = default.expr = NULL
  if (is.expression(lower)) {
    checkExpressionFeasibility(id, lower, envir)
    lower.expr = lower
    lower = -Inf
  } else {
    if (is.numeric(lower) && length(lower) == 1)
      lower = rep(lower, len)
    assertNumeric(lower, min.len = 1L, any.missing = FALSE)
  }
  if (is.expression(upper)) {
    checkExpressionFeasibility(id, upper, envir)
    upper.expr = upper
    upper = Inf
  } else {
    if (is.numeric(upper) && length(upper) == 1)
      upper = rep(upper, len)
    assertNumeric(upper, min.len = 1L, any.missing = FALSE)  
  }
  if (!missing(default) && is.expression(default)) {
    checkExpressionFeasibility(id, default, envir)
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
  if (!is.null(trafo))
    assertFunction(trafo)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  if (any(upper < lower))
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "numericvector", len = len, lower = lower, upper = upper, allow.inf = allow.inf,
    values = NULL, cnames = cnames, default = default, trafo = trafo,
    requires = requires, tunable = tunable)

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
makeIntegerParam = function(id, lower = -Inf, upper = Inf, default, trafo = NULL,
  requires = NULL, tunable = TRUE, envir = NULL) {

  assertString(id)
  # replace all expressions in lower, upper or defaults with feasible values
  # prior to generating the parameter (and afterwards re-substitute them by the
  # original expressions)
  lower.expr = upper.expr = default.expr = NULL
  if (is.expression(lower)) {
    checkExpressionFeasibility(id, lower, envir)
    lower.expr = lower
    lower = -Inf
  } else {
    assertNumber(lower)
  }
  if (is.expression(upper)) {
    checkExpressionFeasibility(id, upper, envir)
    upper.expr = upper
    upper = Inf
  } else {
    assertNumber(upper)
  }
  if (!missing(default) && is.expression(default)) {
    checkExpressionFeasibility(id, default, envir)
    default.expr = default
    if (is.infinite(lower) & is.infinite(upper))
      default = ifelse(lower == upper, lower, 0)
    else
      default = ifelse(is.finite(lower), lower, upper)
  }
  if (!is.null(trafo))
    assertFunction(trafo)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  if (upper < lower)
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "integer", len = 1L, lower = lower, upper = upper,
    values = NULL, cnames = NULL, default = default, trafo = trafo,
    requires = requires, tunable = tunable)

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
  default, trafo = NULL, requires = NULL, tunable = TRUE, envir = NULL) {

  assertString(id)
  len = asInt(len)
  # replace all expressions in lower, upper or defaults with feasible values
  # prior to generating the parameter (and afterwards re-substitute them by the
  # original expressions)
  lower.expr = upper.expr = default.expr = NULL
  if (is.expression(lower)) {
    checkExpressionFeasibility(id, lower, envir)
    lower.expr = lower
    lower = -Inf
  } else {
    if (is.numeric(lower) && length(lower) == 1)
      lower = rep(lower, len)
    assertNumeric(lower, min.len = 1L, any.missing = FALSE)
  }
  if (is.expression(upper)) {
    checkExpressionFeasibility(id, upper, envir)
    upper.expr = upper
    upper = Inf
  } else {
    if (is.numeric(upper) && length(upper) == 1)
      upper = rep(upper, len)
    assertNumeric(upper, min.len = 1L, any.missing = FALSE)
  }
  if (!missing(default) && is.expression(default)) {
    checkExpressionFeasibility(id, default, envir)
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
  if (!is.null(trafo))
    assertFunction(trafo)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  if (any(upper < lower))
    stop("No possible value!")
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "integervector", len = len, lower = lower, upper = upper,
    values = NULL, cnames = cnames, default = default, trafo = trafo,
    requires = requires, tunable = tunable)

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
makeLogicalParam = function(id, default, requires = NULL, tunable = TRUE, envir = NULL) {
  assertString(id)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  assertLogical(tunable, len = 1L)
  # replace the expression in default with feasible values prior to generating
  # the parameters (and then re-substituting them by the original expression)
  default.expr = NULL
  if (!missing(default) && is.expression(default)) {
    checkExpressionFeasibility(id, default, envir)
    default.expr = default
    default = TRUE
  }
  p = makeParam(id = id, type = "logical", len = 1L, lower = NULL, upper = NULL,
    values = values, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = tunable)

  ## re-substitute the expression for default
  if (!is.null(default.expr))
    p$default = default.expr
  return(p)
}

#' @rdname Param
#' @export
makeLogicalVectorParam = function(id, len, cnames = NULL, default, requires = NULL, tunable = TRUE, envir = NULL) {
  assertString(id)
  len = asInt(len)
  if (!is.null(cnames))
    assertCharacter(cnames, len = len, any.missing = FALSE)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  assertLogical(tunable, len = 1L)
  # replace the expression in default with feasible values prior to generating
  # the parameters (and then re-substituting them by the original expression)
  default.expr = NULL
  if (!missing(default) && is.expression(default)) {
    checkExpressionFeasibility(id, default, envir)
    default.expr = default
    default = rep(TRUE, len)
  }
  p = makeParam(id = id, type = "logicalvector", len = len, lower = NULL, upper = NULL,
    values = values, cnames = cnames, default = default, trafo = NULL,
    requires = requires, tunable = tunable)

  ## re-substitute the expression for default
  if (!is.null(default.expr))
    p$default = default.expr
  return(p)
}

#' @rdname Param
#' @export
makeDiscreteParam = function(id, values, trafo = NULL, default, requires = NULL, tunable = TRUE, envir = NULL) {
  assertString(id)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  # replace the expression in values with feasible values prior to generating
  # the parameters (and then re-substituting them by the original expression)
  values.expr = NULL
  if (!missing(values) && is.expression(values)) {
    checkExpressionFeasibility(id, values, envir)
    values.expr = values
    values = list(TRUE)
  } else {
    values = checkValuesForDiscreteParam(id, values)
  }
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "discrete", len = 1L, lower = NULL, upper = NULL,
    values = values, cnames = NULL, default = default, trafo = trafo,
    requires = requires, tunable = tunable)

  ## re-substitute the expression for default
  if (!is.null(values.expr))
    p$values = values.expr
  return(p)
}

#' @rdname Param
#' @export
makeDiscreteVectorParam = function(id, len, values, default, requires = NULL, tunable = TRUE, envir = NULL) {
  assertString(id)
  len = asInt(len)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  # replace the expression in values with feasible values prior to generating
  # the parameters (and then re-substituting them by the original expression)
  values.expr = NULL
  if (!missing(values) && is.expression(values)) {
    checkExpressionFeasibility(id, values, envir)
    values.expr = values
    values = list(TRUE)
  } else {
    values = checkValuesForDiscreteParam(id, values)
  }
  assertLogical(tunable, len = 1L)
  p = makeParam(id = id, type = "discretevector", len = len, lower = NULL, upper = NULL,
    values = values, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = tunable)

  ## re-substitute the expression for default
  if (!is.null(values.expr))
    p$values = values.expr
  return(p)
}

#' @rdname Param
#' @export
makeFunctionParam = function(id, default = default, requires = NULL) {
  assertString(id)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  makeParam(id = id, type = "function", len = 1L, lower = NULL, upper = NULL,
    values = NULL, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = FALSE)
}

#FIXME: what happens if NA is later used for untyped params? because we might interpret this as
# missing value wrt. dependent params
#' @rdname Param
#' @export
makeUntypedParam = function(id, default, requires = NULL, tunable = TRUE) {
  assertString(id)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  makeParam(id = id, type = "untyped", len = 1L, lower = NULL, upper = NULL,
    values = NULL, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = TRUE)
}

#' @rdname Param
#' @export
makeCharacterParam = function(id, default, requires = NULL) {
  assertString(id)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  makeParam(id = id, type = "character", len = 1L, lower = NULL, upper = NULL,
    values = NULL, cnames = NULL, default = default, trafo = NULL,
    requires = requires, tunable = FALSE)
}

#' @rdname Param
#' @export
makeCharacterVectorParam = function(id, len, cnames = NULL, default, requires = NULL) {
  assertString(id)
  len = asInt(len)
  if (!is.null(requires))
    assert(checkClass(requires, "call"), checkClass(requires, "expression"))
  makeParam(id = id, type = "charactervector", len = len, lower = NULL, upper = NULL,
    values = NULL, cnames = cnames, default = default, trafo = NULL,
    requires = requires, tunable = FALSE)
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
  if (any(sapply(values, isScalarNA)))
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

# check whether an expression is feasible
checkExpressionFeasibility = function(id, expr, envir) {
  missing.vars = all.vars(expr)[all.vars(expr) %nin% names(envir)]
  if (length(missing.vars) > 0) {
    res = sprintf("The %s '%s' %s to be defined in 'envir'.",
      ifelse(length(missing.vars) == 1, "parameter", "parameters"),
      paste(missing.vars, collapse = "', '"),
      ifelse(length(missing.vars) == 1, "needs", "need"))
    makeAssertion(id, res = res, collection = NULL)
  }
}
