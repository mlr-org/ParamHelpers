#' @title Get parameter type-strings.
#' @description
#' Returns type strings used in \code{param$type} for certain groups of parameters.
#' @template arg_include_int
#' @template arg_include_logical
#' @return [\code{character}].
#' @name getTypeStrings
#' @rdname getTypeStrings
NULL

#' @export
#' @rdname getTypeStrings
getTypeStringsAll = function() {
  ph$type.strings
}

#' @export
#' @rdname getTypeStrings
getTypeStringsNumeric = function(include.int = TRUE) {
  if (include.int)
    ph$type.strings.numeric
  else
    ph$type.strings.double
}

#' @export
#' @rdname getTypeStrings
getTypeStringsNumericStrict = function() {
  ph$type.strings.double
}

#' @export
#' @rdname getTypeStrings
getTypeStringsInteger = function() {
  ph$type.strings.integer
}

#' @export
#' @rdname getTypeStrings
getTypeStringsCharacter = function() {
  ph$type.strings.character
}

#' @export
#' @rdname getTypeStrings
getTypeStringsDiscrete = function(include.logical = TRUE) {
  if (include.logical)
    c(ph$type.strings.discrete, ph$type.strings.logical)
  else
    ph$type.strings.discrete
}

#' @export
#' @rdname getTypeStrings
getTypeStringsLogical = function() {
  ph$type.strings.logical
}

