#' @title Check if a parameter set is fully numeric without conditional parameters.
#' 
#' @description 
#' Checks if a param set is fully numeric (with \code{\link{isNumeric}}) and has no requires
#' (with \code{\link{hasRequires}})). 
#' 
#' @template arg_parset
#' @template ret_bool
#' @export
isNumericNoReq = function(par.set, include.int = TRUE) {
  isNumeric(par.set, include.int = include.int) && !hasRequires(par.set)
}
