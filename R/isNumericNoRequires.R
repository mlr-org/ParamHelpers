#' @title Check if a parameter set is fully numeric without conditional parameters.
#'
#' @description
#' Checks if a param set is fully numeric (with \code{\link{isNumeric}}) and has no requires
#' (with \code{\link{hasRequires}})).
#'
#' @template arg_parset
#' @param include.int [\code{logical(1)}]\cr
#'   Should integers be seen as numeric? Default is \code{TRUE}.
#' @template ret_bool
#' @export
isNumericNoRequires = function(par.set, include.int = TRUE) {
  assertClass(par.set, "ParamSet")
  assertFlag(include.int)
  isNumeric(par.set, include.int = include.int) && !hasRequires(par.set)
}
