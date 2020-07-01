#' @title modifyParam.R
#' @description copy parameter set to new set, cs, and modify lower
#'              and upper limits of one parameter id.  If parameter is
#'              discrete then change list of permitted values.
#'
#' @param ps - a parameter set, class(ps) = "ParamSet"
#' @param cs - a constraint set, class(ps) = "ParamSet"
#' @param id - id/name of parameter to alter in new set, character
#' @param lower - new lower limit for id - integer, numeric, character or list of character
#' @param upper - new upper limit for id - integer, numeric, character or list of character
#'
#' @return cs - the new constraint set with class(cs) = "ParamSet"
#
#
# usage example:
#     cs = modifyParam(ps, id="power", lower = 333, upper = 333)
#     cs = modifyParam(ps, id="gas", lower = list("Argon", "Nitrogen"))
#  if modifying an existing constraint set
#    cs = modifyParam(ps, cs, id="gas", lower = "Argon", upper="Nitrogen")

modifyParam = function(ps, cs = NULL, id = NULL, lower = NULL, upper = NULL) {
    psParamIDs = getParamIds(ps)
    if (is.null(cs) || !(class(cs) == "ParamSet")) {
        cs = ps
    }
    csParamIDs = getParamIds(cs)
    
    if (!(is.null(id))) {
        if (id %in% psParamIDs) {
            # if param id is in parameter set but not constraint set copy to cs
            if (!(id %in% csParamIDs)) {
                cs[["pars"]][[id]] = ps[["pars"]][[id]]
            }
            # for parameters of type integer or numeric
            if (cs[["pars"]][[id]][["type"]] == "integer" || 
                cs[["pars"]][[id]][["type"]] == "numeric") {
                # if lower is not NULL and within valid limits then set to new lower
                cs[["pars"]][[id]][["lower"]] = validNum(ps, id, lower, "lower")
                # if upper is not NULL and within valid limits then set to new upper
                cs[["pars"]][[id]][["upper"]] = validNum(ps, id, upper, "upper")
            # for parameters of type discrete
            } else if (cs[["pars"]][[id]][["type"]] == "discrete") {
                values = c()
                # make list of valid discrete values for parameter id
                for (value in ps[["pars"]][[id]][["values"]]) {
                    values = append(values, value)
                }
                appVal = 0L
                # if lower is not NULL and in ps's set of param IDs then copy value to cs
                if (!(is.null(lower))) {
                    if (class(lower) == "character" && length(lower) == 1) {
                        if (lower %in% values) {
                            appVal = 2L
                        } else {
                            cat("Warning:",lower,"not a valid value for",id,"\b, using values from parameter set\n")
                            appVal = 1L
                        }
                    } else if (class(lower) == "list") {
                        apndLow = list()
                        for (low in lower) {
                            if (low %in% values) {
                                apndLow = append(apndLow, low)
                                appVal = 2L
                            } else {
                                cat("Warning:",low,"not a valid value for",id,"\n")
                            }
                        }
                    }
                }
                # if upper is not NULL and in ps's set of param IDs then copy value to cs
                if (!(is.null(upper))) {
                    if (class(upper) == "character" && length(upper) == 1L) {
                        if (upper %in% values) {
                            if (appVal == 0L) appVal = 3L
                            if (appVal == 2L) appVal = 4L
                        } else {
                            cat("Warning:",upper,"not a valid value for",id,"\b, using values from parameter set\n")
                            appVal = 1L
                        }
                    } else if (class(upper) == "list") {
                        apndUp = list()
                        for (up in upper) {
                            if (up %in% values) {
                                apndUp = append(apndUp, up)
                                if (appVal == 0L) appVal = 3L
                                if (appVal == 2L) appVal = 4L
                            } else {
                                cat("Warning:",up,"not a valid value for",id,"\n")
                            }   
                        }
                    }
                }
                # determine how to append values to a discrete parameter
                if (appVal >= 2L) {
                    cs[["pars"]][[id]][["values"]] = NULL
                    if (appVal == 2L) {
                        if (class(lower) == "character") apnd = list(lower)
                        if (class(lower) == "list") apnd = apndLow
                    } else if (appVal == 3L) {
                        if (class(upper) == "character") apnd = list(upper)
                        if (class(upper) == "list") apnd = apndUp
                    } else if (appVal == 4L) {
                        if (class(lower) == "list") {
                            apnd = append(lower, upper)
                        } else if (class(lower) == "character" && class(upper) == "list") {
                            apnd = append(upper, lower)
                        } else {
                            apnd = list(lower, upper)
                        }
                    }
                    apnd = unique(apnd)
                    cs[["pars"]][[id]][["values"]] = apnd
                    names(cs[["pars"]][[id]][["values"]]) = apnd
                } 
            # other types of parameters not supported
            } else {
                print("modifyParam only implemented for types of integer, numeric & discrete")
            }
        } else {
            cat("Warning:",id,"not in parameter set, ignoring constraint\n")
        }
    }

    return(cs)
}

# if limitVal is within limts and "numeric" return limitVal, otherwise return ps limit
validNum = function(ps, id, limitVal, limit) {
    ret = ps[["pars"]][[id]][[limit]]
    if (!(is.null(limitVal))) {
        if (class(limitVal) == "numeric") {
            if (limitVal >= ps[["pars"]][[id]][["lower"]] && 
                limitVal <= ps[["pars"]][[id]][["upper"]]) {
                ret = limitVal
            } else {
                cat("Warning:",limitVal,"is outside parameter limits, using limit from parameter set\n")
            }
        } else {
            cat("Warning: integer and numeric parameters must have class(value) = numeric\n")

        }
    }
    return(ret)
}


