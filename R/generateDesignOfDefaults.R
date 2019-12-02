#' @title Generates a design with the defaults of a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  `numeric`  \cr
#'  integer(vector)   \tab  `integer`  \cr
#'  discrete(vector)  \tab  `factor` (names of values = levels) \cr
#'  logical(vector)   \tab  `logical`
#' }
#' This will create a design containing only one point at the default values of
#' the supplied param set. In most cases you will combine the resulting
#' `data.frame` with a different generation function e.g. [generateDesign()],
#' [generateRandomDesign()] or [generateGridDesign()]. This is useful to force
#' an evaluation at the default location of the parameters while still
#' generating a design. Parameters default values, whose conditions (`requires`)
#' are not fulfilled will be set to `NA` in the result.
#' @template arg_parset
#' @template arg_trafo
#' @template ret_gendes_df
#' @export
generateDesignOfDefaults = function(par.set, trafo = FALSE) {

  doBasicGenDesignChecks(par.set)
  assertFlag(trafo)

  pars = par.set$pars

  defaults = getDefaults(par.set)
  diff = setdiff(names(pars), names(defaults))
  if (length(diff) > 0) {
    stopf("No default parameter setting for: %s", collapse(diff))
  }

  # check if one or more default values are special values
  special.default = mapply(FUN = function(p, value) isSpecialValue(par.set$pars[[p]], value),
    p = names(defaults), value = defaults, SIMPLIFY = TRUE)
  if (any(special.default)) {
    stopf("special.vals as default for Parameter(s): %s", collapse(names(defaults)[special.default]))
  }

  # convert discrete value here to names, to we can stuff them into a df
  defaults = mapply(FUN = function(p, d) {
    if (isDiscrete(p, include.logical = FALSE)) discreteValueToName(p, d) else d
  }, par.set$pars, defaults, SIMPLIFY = FALSE)


  res = listToDfOneRow(defaults)

  # now trafo and set params whose conditions are not fulfilled to NA
  #
  if (trafo) {
    res = applyTrafos(res, pars)
  }
  if (hasRequires(par.set)) {
    res = setRequiresToNA(res, pars)
  }

  res = fixDesignFactors(res, par.set)
  res = fixDesignVarTypes(res, par.set)

  attr(res, "trafo") = trafo
  return(res)
}
