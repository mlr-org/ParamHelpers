#' @rdname LearnerParam
#' @export
makeNumericLearnerParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE, default,
  when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  p = makeNumericParam(id, lower, upper, allow.inf = allow.inf, default = default, requires = requires, tunable = tunable, special.vals = special.vals)
  learnerParamFromParam(p, when)
}

#' @rdname LearnerParam
#' @export
makeNumericVectorLearnerParam = function(id, len = as.integer(NA), lower = -Inf,
  upper = Inf, allow.inf = FALSE, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  ## makeNumericVectorParam is not able to use expressions in 'length', thus we
  ## do a dirty-hack, in which we define len = 1L for the construction of p and
  ## afterwards re-insert the expression for the length
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len, na.ok = TRUE)
  }

  if (is.na(len))
    p = makeVectorParamNALength(makeNumericVectorParam, default = default,
      id = id, lower = lower, upper = upper, allow.inf = allow.inf, requires = requires, tunable = tunable, special.vals = special.vals)
  else
    p = makeNumericVectorParam(id, len = len, lower = lower, upper = upper, allow.inf = allow.inf, default =  default,
      requires = requires, tunable = tunable, special.vals = special.vals)
  p = learnerParamFromParam(p, when)
  ## re-insert the expression (if it existed in the beginning)
  if (!is.null(len.expr))
    p$len = len.expr
  else
    p$len = len
  return(p)
}


#' @rdname LearnerParam
#' @export
makeIntegerLearnerParam = function(id, lower = -Inf, upper = Inf,
  default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  p = makeIntegerParam(id, lower, upper, default = default, requires = requires, tunable = tunable, special.vals = special.vals)
  learnerParamFromParam(p, when)
}

#' @rdname LearnerParam
#' @export
makeIntegerVectorLearnerParam = function(id, len = as.integer(NA), lower = -Inf,
  upper = Inf, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  ## makeIntegerVectorParam is not able to use expressions in 'length', thus we
  ## do a dirty-hack, in which we define len = 1L for the construction of p and
  ## afterwards re-insert the expression for the length
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len, na.ok = TRUE)
  }
  if (is.na(len))
    p = makeVectorParamNALength(makeIntegerVectorParam, default = default,
      id = id, lower = lower, upper = upper, requires = requires, tunable = tunable, special.vals = special.vals)
  else
    p = makeIntegerVectorParam(id, len = len, lower = lower, upper = upper, default = default,
      requires = requires, tunable = tunable, special.vals = special.vals)
  p = learnerParamFromParam(p, when)
  ## re-insert the expression (if it existed in the beginning)
  if (!is.null(len.expr))
    p$len = len.expr
  else
    p$len = len
  return(p)
}

#' @rdname LearnerParam
#' @export
makeDiscreteLearnerParam = function(id, values, default,
  when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  p = makeDiscreteParam(id, values, default = default, requires = requires, tunable = tunable, special.vals = special.vals)
  learnerParamFromParam(p, when)
}

#' @rdname LearnerParam
#' @export
makeDiscreteVectorLearnerParam = function(id, len = as.integer(NA), values, default,
  when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  ## makeDiscreteVectorParam is not able to use expressions in 'length', thus we
  ## do a dirty-hack, in which we define len = 1L for the construction of p and
  ## afterwards re-insert the expression for the length
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len, na.ok = TRUE)
  }

  if (is.na(len))
    p = makeVectorParamNALength(makeDiscreteVectorParam, default = default,
      id = id, values = values, requires = requires, tunable = tunable, special.vals = special.vals)
  else
    p = makeDiscreteVectorParam(id, len = len, values = values, default = default, requires = requires,
      tunable = tunable, special.vals = special.vals)
  p = learnerParamFromParam(p, when)
  ## re-insert the expression (if it existed in the beginning)
  if (!is.null(len.expr))
    p$len = len.expr
  else
    p$len = len
  return(p)
}

#' @rdname LearnerParam
#' @export
makeLogicalLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {

  p = makeLogicalParam(id, default = default, requires = requires, tunable = tunable, special.vals = special.vals)
  learnerParamFromParam(p, when)
}

#' @rdname LearnerParam
#' @export
makeLogicalVectorLearnerParam = function(id, len = as.integer(NA), default, when = "train",
  requires = NULL, tunable = TRUE, special.vals = list()) {

  ## makeLogicalVectorParam is not able to use expressions in 'length', thus we
  ## do a dirty-hack, in which we define len = 1L for the construction of p and
  ## afterwards re-insert the expression for the length
  len.expr = NULL
  if (is.expression(len)) {
    len.expr = len
    len = 1L
  } else {
    len = asInt(len, na.ok = TRUE)
  }

  if (is.na(len))
    p = makeVectorParamNALength(makeLogicalVectorParam, default = default,
      id = id, requires = requires, tunable = tunable, special.vals = special.vals)
  else
    p = makeLogicalVectorParam(id, len = len, default = default, requires = requires, tunable = tunable, special.vals = special.vals)
  p = learnerParamFromParam(p, when)
  ## re-insert the expression (if it existed in the beginning)
  if (!is.null(len.expr))
    p$len = len.expr
  else
    p$len = len
  return(p)
}

#' @rdname LearnerParam
#' @export
makeUntypedLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  p = makeUntypedParam(id, default = default, requires = requires, tunable = tunable, special.vals = special.vals)
  learnerParamFromParam(p, when)
}

#' @rdname LearnerParam
#' @export
makeFunctionLearnerParam = function(id, default, when = "train", requires = NULL, special.vals = list()) {
  p = makeFunctionParam(id, default = default, requires = requires, special.vals = special.vals)
  learnerParamFromParam(p, when)
}

learnerParamFromParam = function(p, when) {
  assertChoice(when, c("train", "predict", "both"))
  makeLearnerParam(p, when)
}

makeVectorParamNALength = function(fun, default, ...)  {
  len = if (missing(default)) 1L else length(default)
  fun(len = len, default = default, ...)
}
