#' @rdname Param
#' @export
makeNumericParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE,
  default, trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list()) {

  makeParam(id = id, type = "numeric", learner.param = FALSE, lower = lower,
    upper = upper, allow.inf = allow.inf,
    default = default, trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeNumericVectorParam = function(id, len, lower = -Inf, upper = Inf, cnames = NULL,
  allow.inf = FALSE, default, trafo = NULL, requires = NULL,
  tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numericvector", learner.param = FALSE, len = len, lower = lower,
    upper = upper, cnames = cnames, allow.inf = allow.inf,
    default = default, trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeIntegerParam = function(id, lower = -Inf, upper = Inf, default, trafo = NULL,
  requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integer", learner.param = FALSE, lower = lower,
    upper = upper, default = default,
    trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeIntegerVectorParam = function(id, len, lower = -Inf, upper = Inf, cnames = NULL,
  default, trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integervector", learner.param = FALSE, len = len, lower = lower, upper = upper,
    cnames = cnames, default = default, trafo = trafo,
    requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeLogicalParam = function(id, default, requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logical", learner.param = FALSE,
    values = values, default = default,
    trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeLogicalVectorParam = function(id, len, cnames = NULL, default,
  requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logicalvector", learner.param = FALSE, len = len,
    values = values, cnames = cnames, default = default,
    trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeDiscreteParam = function(id, values, trafo = NULL, default,
  requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discrete", learner.param = FALSE,
    values = values, default = default,
    trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeDiscreteVectorParam = function(id, len, values, default, requires = NULL,
  tunable = TRUE, special.vals = list()) {

  makeParam(id = id, type = "discretevector", learner.param = FALSE, len = len,
    values = values, default = default,
    trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeFunctionParam = function(id, default = default, requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "function", learner.param = FALSE,
    values = NULL, default = default, trafo = NULL,
    requires = requires, tunable = FALSE, special.vals = special.vals)
}

#FIXME: what happens if NA is later used for untyped params? because we might interpret this as
# missing value wrt. dependent params
#' @rdname Param
#' @export
makeUntypedParam = function(id, default, requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "untyped", learner.param = FALSE,
    values = NULL, default = default, trafo = NULL,
    requires = requires, tunable = TRUE, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeCharacterParam = function(id, default, requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "character", learner.param = FALSE,
    default = default, trafo = NULL,
    requires = requires, tunable = FALSE, special.vals = special.vals)
}

#' @rdname Param
#' @export
makeCharacterVectorParam = function(id, len, cnames = NULL, default,
  requires = NULL, special.vals = list()) {

  makeParam(id = id, type = "charactervector", learner.param = FALSE, len = len,
    cnames = cnames, default = default,
    trafo = NULL, requires = requires, tunable = FALSE, special.vals = special.vals)
}

