#' @rdname LearnerParam
#' @export
makeNumericLearnerParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE, default,
  when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numeric", learner.param = TRUE, lower = lower,
    upper = upper, allow.inf = allow.inf,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeNumericVectorLearnerParam = function(id, len = as.integer(NA), lower = -Inf,
  upper = Inf, allow.inf = FALSE, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numericvector", learner.param = TRUE, len = len, lower = lower,
    upper = upper, allow.inf = allow.inf,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}


#' @rdname LearnerParam
#' @export
makeIntegerLearnerParam = function(id, lower = -Inf, upper = Inf,
  default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integer", learner.param = TRUE, lower = lower,
    upper = upper,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeIntegerVectorLearnerParam = function(id, len = as.integer(NA), lower = -Inf,
  upper = Inf, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integervector", learner.param = TRUE, len = len, lower = lower,
    upper = upper,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeDiscreteLearnerParam = function(id, values, default,
  when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discrete", learner.param = TRUE, values = values,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeDiscreteVectorLearnerParam = function(id, len = as.integer(NA), values, default,
  when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discretevector", learner.param = TRUE, len = len, values = values,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeLogicalLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logical", learner.param = TRUE, values = values,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeLogicalVectorLearnerParam = function(id, len = as.integer(NA), default, when = "train",
  requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logicalvector", learner.param = TRUE, len = len,values = values,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeUntypedLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "untyped", learner.param = TRUE,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

#' @rdname LearnerParam
#' @export
makeFunctionLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "function", learner.param = TRUE,
    default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}


