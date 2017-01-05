#' @title Generates a design with the defaults of a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  \code{numeric}  \cr
#'  integer(vector)   \tab  \code{integer}  \cr
#'  discrete(vector)  \tab  \code{factor} (names of values = levels) \cr
#'  logical(vector)   \tab  \code{logical}
#' }
#' This will create a design containing only one point at the default values of the supplied param set.
#' In most cases you will combine the resulting \code{data.frame} with a different generation function
#' e.g. \code{\link{generateDesign}}, \code{\link{generateRandomDesign}} or \code{\link{generateGridDesign}}.
#' This is useful to force an evaluation at the default location of the parameters while still generating
#' a design.
#' Parameters default values, whose conditions (\code{requires}) are not fulfilled will be set to \code{NA}
#' in the result.
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
  if (length(diff) > 0)
    stopf("No default parameter setting for: %s", collapse(diff))

  # check if one or more default values are special values
  special.default = mapply(FUN = function(p, value) isSpecialValue(par.set$pars[[p]], value),
    p = names(defaults), value = defaults, SIMPLIFY = TRUE)
  if (any(special.default))
    stopf("special.vals as default for Parameter(s): %s", collapse(names(defaults)[special.default]))

  # convert discrete value here to names, to we can stuff them into a df
  defaults = mapply(FUN = function(p, d) {
    if (isDiscrete(p, include.logical = FALSE)) discreteValueToName(p, d) else d
  }, par.set$pars, defaults, SIMPLIFY = FALSE)


  res = listToDfOneRow(defaults)

  # now trafo and set params whose conditions are not fulfilled to NA
  #
  # FIXME: this is basically a copy-paste from generateGridDesign and generateDesign
  # we should probably refactor this operation! the whole code sucks here!
  if (trafo || hasRequires(par.set)) {
    lens = getParamLengths(par.set)
    # the following lines are mainly copy paste from generateDesign
    types.df = getParamTypes(par.set, df.cols = TRUE)
    types.int = convertTypesToCInts(types.df)
    # ignore trafos if the user did not request transformed values
    trafos = if (trafo)
      lapply(pars, function(p) p$trafo)
    else
      replicate(length(pars), NULL, simplify = FALSE)
    par.requires = getRequirements(par.set, remove.null = FALSE)
    res = convertDataFrameCols(res, factors.as.char = TRUE)
    res = .Call(c_trafo_and_set_dep_to_na, res, types.int, names(pars), lens, trafos, par.requires, new.env())
  }

  res = fixDesignFactors(res, par.set)
  res = fixDesignVarTypes(res, par.set)

  attr(res, "trafo") = trafo
  return(res)
}
