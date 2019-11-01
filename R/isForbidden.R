#' @title Check whether parameter setting lies in forbidden region of parameter
#'   set.
#'
#' @description Parameter sets without a forbidden region always return `FALSE`.
#'
#' @template arg_parset
#' @param x (named `list`) \cr
#'   Parameter setting to check.
#' @return `logical(1)`.
#' @export
isForbidden = function(par.set, x) {
  assertClass(par.set, "ParamSet")
  # FIXME: check for correct names here
  assertList(x)
  if (!hasForbidden(par.set)) {
    return(FALSE)
  }
  return(eval(par.set$forbidden, envir = x))
}
