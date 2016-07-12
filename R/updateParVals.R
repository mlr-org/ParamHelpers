#' @title Insert par.vals to old ones with meeting requirements
#' @description
#'   Update the values of a given parameter setting with a new parameter setting.
#'   Settings that do not meet the requirements anymore will be deleted from the first given parameter setting.
#'
#' @template arg_parset
#' @param old.par.vals [\code{list}]\cr
#'   Param Values to be updated.
#' @param new.par.vals [\code{list}]\cr
#'   New Param Values to update the \code{old.par.vals}.
#' @param warn [\code{logical}]\cr
#'   Whether a warning should be shown, if a param setting from \code{old.par.vals} is dropped.
#'   Default is \code{FALSE}.
#' @return [\code{list}].
#' @export
updateParVals = function(par.set, old.par.vals, new.par.vals, warn = FALSE) {
  assertList(old.par.vals)
  assertList(new.par.vals)
  assertNamed(old.par.vals)
  assertNamed(new.par.vals)
  assertClass(par.set, "ParamSet")
  repeat {
    # we repeat to include parameters which depend on each other by requirements
    candidate.par.names = setdiff(names(old.par.vals), names(new.par.vals))
    for (pn in candidate.par.names) {
      if (all(getRequiredParamNames(par.set$pars[[pn]]) %in% names(new.par.vals)) && requiresOk(par.set$pars[[pn]], new.par.vals)) {
        new.par.vals[pn] = old.par.vals[pn]
      } else if (warn){
        warningf("ParamSetting %s was dropped.", convertToShortString(old.par.vals[pn]))
      }
    }
    # break if no changes were made
    if (identical(candidate.par.names, setdiff(names(old.par.vals), names(new.par.vals)))) break
  }
  return(new.par.vals)
}
