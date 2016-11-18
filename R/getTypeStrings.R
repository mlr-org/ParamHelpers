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
getTypeStringsNumeric = function(include.int = TRUE) {
  if (include.int)
    c("numeric", "numericvector", "integer", "integervector")
  else
    c("numeric", "numericvector")
}

#' @export
#' @rdname getTypeStrings
getTypeStringsNumericStrict = function() {
  c("numeric", "numericvector")
}

#' @export
#' @rdname getTypeStrings
getTypeStringsInteger = function() {
  c("integer", "integervector")
}

#' @export
#' @rdname getTypeStrings
getTypeStringsDiscrete = function(include.logical = TRUE) {
  if (include.logical)
    c("discrete", "discretevector", "logical", "logicalvector")
  else
    c("discrete", "discretevector")
}

