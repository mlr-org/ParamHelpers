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
#' @template arg_parset
#' @template arg_trafo
#' @template ret_gendes_df
#' @export
generateDefaultDesign = function(par.set, trafo = FALSE) {
  doBasicGenDesignChecks(par.set)
  assertFlag(trafo)
  
  pars = par.set$pars
  
  defaults = getDefaults(par.set)
  diff = setdiff(names(pars), names(defaults))
  if (length(diff) > 0)
    stop(sprintf("No default parameter setting for %s", paste(diff, collapse = ", ")))
  
  # apply transformations to parameters
  if (trafo)
    defaults = mapply(function(d, p) ifelse(is.null(p$trafo), d, p$trafo(d)), 
      d = defaults, p = pars)
  
  
  # convert list to data.frame with one column.
  res = data.frame(lapply(seq_along(defaults), function(x) t(unlist(defaults[x]))))
  res = fixDesignFactors(res, par.set)
  attr(res, "trafo") = trafo
  return(res)
  
}