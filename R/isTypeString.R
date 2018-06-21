#' @title Check if type string is of certain type.
#'
#' @description
#' \code{TRUE} iff the type string is a certain type,
#' e.g. \code{isIntegerTypeString} checks if we have \dQuote{integer} or \dQuote{integervector},
#' and \code{isVectorTypeString} check if we have \dQuote{*vector}.
#'
#' @template arg_typestring
#' @template arg_include_int
#' @template arg_include_logical
#' @template ret_bool
#' @name isTypeString
#' @rdname isTypeString
NULL

#' @export
#' @rdname isTypeString
isNumericTypeString = function(type, include.int = TRUE) {
  type %fin% getTypeStringsNumeric(include.int)
}

#' @export
#' @rdname isTypeString
isIntegerTypeString = function(type) {
  type %fin% getTypeStringsInteger()
}

#' @export
#' @rdname isTypeString
isCharacterTypeString = function(type) {
  type %fin% getTypeStringsCharacter()
}

#' @export
#' @rdname isTypeString
isDiscreteTypeString = function(type, include.logical = TRUE) {
  type %fin% getTypeStringsDiscrete(include.logical)
}

#' @export
#' @rdname isTypeString
isLogicalTypeString = function(type) {
  type %fin% getTypeStringsLogical()
}

#' @export
#' @rdname isTypeString
isVectorTypeString = function(type) {
  endsWith(type, "vector")
}
