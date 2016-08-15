#' @title Insert par.vals to old ones with meeting requirements
#' @description
#'   Update the values of a given parameter setting with a new parameter setting.
#'   Settings that do not meet the requirements anymore will be deleted from the first given parameter setting.
#'   Default values of the Param Set are respected to chek if the new param settings meet the requirements.
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
  assertList(old.par.vals, names = "named")
  assertList(new.par.vals, names = "named")
  assertClass(par.set, "ParamSet")
  assertFlag(warn)
  default.par.vals = getDefaults(par.set)
  # First we extend both par.vals lists with the defaults to get the fully requirements meeting par.vals lists
  old.with.defaults = updateParVals2(par.set = par.set, old.par.vals = default.par.vals, new.par.vals = old.par.vals)
  updated.old = attr(old.with.defaults, "updated")
  new.with.defaults = updateParVals2(par.set = par.set, old.par.vals = default.par.vals, new.par.vals = new.par.vals)
  updated.new = attr(new.with.defaults, "updated")
  # updated in old but not present in new
  respect.old.updated = setdiff(names(updated.old)[updated.old], names(new.par.vals))
  # new.defaults that can be updated
  new.candidates = setdiff(names(new.with.defaults), respect.old.updated)
  updated.par.vals = updateParVals2(par.set = par.set, old.par.vals = old.with.defaults, new.par.vals = new.with.defaults[new.candidates])
  # Find out which parmam names were kept in both update processes
  # this indicates that this was a default and we don't need it, as it is still a valid default.
  both.updated = union(names(updated.new)[updated.new], names(updated.old)[updated.old])
  result = updated.par.vals[names(updated.par.vals) %in% both.updated]
  # order as in par.set
  # result = result[match(names(result), getParamIds(par.set))]
  if (warn) {
    # detect dropped param settings:
    warningf("ParamSettings (%s) were dropped.", convertToShortString(old.par.vals[names(old.par.vals) %nin% names(result)]))
  }
  return(result)
}

updateParVals2 = function(par.set, old.par.vals, new.par.vals) {
  updated = setNames(rep(TRUE, length(new.par.vals)), names(new.par.vals))
  repeat {
    # we include parameters of the old.par.vals if they meet the requirements
    # we repeat because some parameters of old.par.vals might only meet the requirements after we added others. (chained requirements)
    candidate.par.names = setdiff(names(old.par.vals), names(new.par.vals))
    for (pn in candidate.par.names) {
      # If all requirement parameters for the candidate are in the new.par.vals and if the requirements are met
      if (isTRUE(try(requiresOk(par.set$pars[[pn]], c(new.par.vals, old.par.vals[pn])), silent = TRUE))) {
        # keep old.par.val in new.par.vals as it meets the requirements
        new.par.vals[pn] = old.par.vals[pn] 
        updated[pn] = FALSE
      }
    }
    # break if no changes were made
    if (identical(candidate.par.names, setdiff(names(old.par.vals), names(new.par.vals)))) break
  }
  setAttribute(new.par.vals, "updated", updated)
}