#' @title Generates a random design for a parameter set.
#'
#' @description
#' The following types of columns are created:
#' \tabular{ll}{
#'  numeric(vector)   \tab  `numeric`  \cr
#'  integer(vector)   \tab  `integer`  \cr
#'  discrete(vector)  \tab  `factor` (names of values = levels) \cr
#'  logical(vector)   \tab  `logical`
#' }
#' If you want to convert these, look at [BBmisc::convertDataFrameCols()]. For
#' discrete vectors the levels and their order will be preserved, even if not
#' all levels are present.
#'
#' The algorithm simply calls [sampleValues()] and arranges the result in a
#' data.frame.
#'
#' Parameters are trafoed (potentially, depending on the setting of argument
#' `trafo`); dependent parameters whose constraints are unsatisfied are set to
#' `NA` entries.
#'
#' `generateRandomDesign` will NOT work if there are dependencies over multiple
#' levels of parameters and the dependency is only given with respect to the
#' \dQuote{previous} parameter. A current workaround is to state all
#' dependencies on all parameters involved. (We are working on it.)
#'
#' Note that if you have trafos attached to your params, the complete creation
#' of the design (except for the detection of invalid parameters w.r.t to their
#' `requires` setting) takes place on the UNTRANSFORMED scale. So this function
#' samples from a uniform density over the param space on the UNTRANSFORMED
#' scale, but not necessarily the transformed scale.
#'
#' @template arg_gendes_n
#' @template arg_parset
#' @template arg_trafo
#' @template ret_gendes_df
#' @export
generateRandomDesign = function(n = 10L, par.set, trafo = FALSE) {

  doBasicGenDesignChecks(par.set)
  des = sampleValues(par.set, n, discrete.names = TRUE, trafo = trafo)

  # FIXME: all next lines are sloooow in R I guess. C?
  elementsToDf = function(x) {
    Map(function(p, v) {
      # blow up scalar NAs
      if (isScalarNA(v)) {
        v = as.data.frame(t(rep(v, p$len)))
      } else {
        as.data.frame(t(v))
      }
    }, par.set$pars, x)
  }
  des = lapply(des, elementsToDf)
  des = lapply(des, do.call, what = cbind)
  des = do.call(rbind, des)
  colnames(des) = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  des = fixDesignFactors(des, par.set)
  attr(des, "trafo") = trafo
  return(des)
}
