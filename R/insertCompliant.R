#' @title Insert par.vals to old ones with meeting requirements
#' @description 
#'   Update the values of a given parameter setting with a new parameter setting.
#'   Settings that do not meet the requirements anymore will be deleted from the first given parameter setting.
#' @param old.par.vals [\code{list}]\cr
#'   Param Values to be updated.
#' @param new.par.vals [\code{list}]\cr
#'   New Param Values to update the \code{old.par.vals}.
#' @return [\code{list}]
#' @template arg_parset
#' @export
insertCompliant = function(old.par.vals, new.par.vals, par.set) {
  assertList(old.par.vals)
  assertList(new.par.vals)
  assertNamed(old.par.vals)
  assertNamed(new.par.vals)
  assertClass(par.set, "ParamSet")
  repeat {
    # we repeat to include parameters which depend on each other by requirements
    new.pars = setdiff(names(old.par.vals), names(new.par.vals))
    for (pn in new.pars) {
      if (isTRUE(try(requiresOk(par.set$pars[[pn]], new.par.vals), silent = TRUE))) {
        new.par.vals[pn] = old.par.vals[pn]
      }
    }
    # break if no changes were made
    if (identical(new.pars, setdiff(names(old.par.vals), names(new.par.vals)))) break
  }
  return(new.par.vals)
}
