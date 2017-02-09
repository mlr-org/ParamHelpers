#' @title Check if a parameter set is numeric and without any requires.
#' 
#' @description 
#' Checks if a par.set is fully numeric (with \code{\link{isNumeric}}) and has no requires
#' (with \code{\link{hasRequires}})). 
#' 
#' @template arg_parset
#' @template ret_bool
#' @export
isSimpleNumeric = function(par.set) {
  isNumeric(par.set, include.int = TRUE) && !hasRequires(par.set)
}