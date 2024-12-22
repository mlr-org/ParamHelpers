#' @import checkmate
#' @import methods
#' @import stats
#' @importFrom fastmatch %fin% fmatch
#' @rawNamespace import(BBmisc, except = isFALSE)
NULL

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
}

ph = new.env(parent = emptyenv())
ph$type.strings.integer = c("integer", "integervector")
ph$type.strings.double = c("numeric", "numericvector")
ph$type.strings.character = c("character", "charactervector")
ph$type.strings.logical = c("logical", "logicalvector")
ph$type.strings.discrete = c("discrete", "discretevector")
ph$type.strings.numeric = c(ph$type.strings.integer, ph$type.strings.double)
ph$type.strings = c(ph$type.strings.integer, ph$type.strings.double, ph$type.strings.character, ph$type.strings.logical, ph$type.strings.discrete, "untyped", "function")

ph$convert.param.types.from = c("numericvector", "integervector", "discrete", "discretevector", "logicalvector", "charactervector")
ph$convert.to.ctypes = c("numeric", "integer", "factor", "logical", "character")
ph$value.component.names = c("numericvector", "integervector", "logicalvector")
globalVariables(".data")
