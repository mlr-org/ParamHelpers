#' @title Insert par.vals to old ones with meeting requirements
#' @description Update the values of a given parameter setting with a new
#' parameter setting. Settings that do not meet the requirements anymore will be
#' deleted from the first given parameter setting. Default values of the Param
#' Set are respected to check if the new param settings meet the requirements.
#'
#' @template arg_parset
#' @param old.par.vals [list]\cr
#'   Param Values to be updated.
#' @param new.par.vals [list]\cr
#'   New Param Values to update the `old.par.vals`.
#' @param warn [logical]\cr
#'   Whether a warning should be shown, if a param setting from `old.par.vals`
#'   is dropped. Default is `FALSE`.
#' @return [`list`].
#' @export
updateParVals = function(par.set, old.par.vals, new.par.vals, warn = FALSE) {
  assertList(old.par.vals, names = "named")
  assertList(new.par.vals, names = "named")
  assertClass(par.set, "ParamSet")

  # we might want to check requires with defaults that are not overwritten by new.par.vals
  usable.defaults = getDefaults(par.set)
  usable.defaults = usable.defaults[names(usable.defaults) %nin% names(new.par.vals)]
  repeat {
    # we repeat to include parameters which depend on each other by requirements
    # candidates are params of the old par.vals we might still need.
    candidate.par.names = setdiff(names(old.par.vals), names(new.par.vals))
    for (pn in candidate.par.names) {
      # If all requirement parameters for the candidate are in the new.par.vals and if the requirements are met
      if (all(getRequiredParamNames(par.set$pars[[pn]]) %in% names(new.par.vals)) && requiresOk(par.set$pars[[pn]], new.par.vals)) {
        new.par.vals[pn] = old.par.vals[pn] # keep old.par.val in new.par.vals as it meets the requirements
      } else if (all(getRequiredParamNames(par.set$pars[[pn]]) %in% names(usable.defaults)) && requiresOk(par.set$pars[[pn]], usable.defaults)) {
        new.par.vals[pn] = old.par.vals[pn] # keep old.par.val as it meets requirement via defaults
      } else if (warn) {
        # otherwise we can drop the old par.val because it does not meet the requirements.
        warningf("ParamSetting %s was dropped.", convertToShortString(old.par.vals[pn]))
      }
    }
    # break if no changes were made
    if (identical(candidate.par.names, setdiff(names(old.par.vals), names(new.par.vals)))) break
  }
  return(new.par.vals)
}
