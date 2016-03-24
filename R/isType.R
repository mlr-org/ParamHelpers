#' Check parameter / parameter set contain ONLY a certain type.
#'
#' An empty param set is considered to be of all types.
#'
#' @template arg_par_or_set
#' @template arg_include_int
#' @template arg_include_logical
#' @return [\code{logical(1)}].
#' @name isType
#' @rdname isType
NULL

#' @export
#' @rdname isType
isNumeric = function(par, include.int = TRUE) {
  assert(checkClass(par, "Param"), checkClass(par, "ParamSet"))
  UseMethod("isNumeric")
}

#' @export
isNumeric.ParamSet = function(par, include.int = TRUE) {
  all(sapply(par$pars, isNumeric.Param, include.int = include.int))
}

#' @export
isNumeric.Param = function(par, include.int = TRUE) {
  return(par$type %in% getNumericTypes(include.int))
}

#' @export
#' @rdname isType
isDiscrete = function(par, include.logical = TRUE) {
  assert(checkClass(par, "Param"), checkClass(par, "ParamSet"))
  UseMethod("isDiscrete")
}

#' @export
isDiscrete.ParamSet = function(par, include.logical = TRUE) {
  return(hasAllParamsOfTypes(par, types = getDiscreteTypes(include.logical)))
}

#' @export
isDiscrete.Param = function(par, include.logical = TRUE) {
  return(par$type %in% getDiscreteTypes(include.logical))
}

#' @export
#' @rdname isType
isInteger = function(par) {
  assert(checkClass(par, "Param"), checkClass(par, "ParamSet"))
  UseMethod("isInteger")
}

#' @export
isInteger.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("integer", "integervector")))
}

#' @export
isInteger.Param = function(par) {
  return(par$type %in% c("integer", "integervector"))
}

#' @export
#' @rdname isType
isLogical = function(par) {
  assert(checkClass(par, "Param"), checkClass(par, "ParamSet"))
  UseMethod("isLogical")
}

#' @export
isLogical.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("logical", "logicalvector")))
}

#' @export
isLogical.Param = function(par) {
  return(par$type %in% c("logical", "logicalvector"))
}

#' @export
#' @rdname isType
isCharacter = function(par) {
  assert(checkClass(par, "Param"), checkClass(par, "ParamSet"))
  UseMethod("isCharacter")
}

#' @export
isCharacter.ParamSet = function(par) {
  return(hasAllParamsOfTypes(par, types = c("character", "charactervector")))
}

isCharacter.Param = function(par) {
  return(par$type %in% c("character", "charactervector"))
}
