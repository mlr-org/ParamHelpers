#' @title Split up a vector param into a list of ind
#'
#' @description
#' Useful if vectors are included.
#'
#' @template arg_par_or_set
#' @param repeated [\code{logical(1)}]\cr
#'   Should ids be repeated length-times if parameter is a vector?
#'   Default is \code{FALSE}.
#' @param with.nr [\code{logical(1)}]\cr
#'   Should number from 1 to length be appended to id if \code{repeated} is \code{TRUE}?
#'   Otherwise ignored.
#'   Default is \code{FALSE}.
#' @return [\code{character}].
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u"),
#'   makeIntegerVectorParam("v", len = 2)
#' )
#' getParamIds(ps)
#' getParamIds(ps, repeated = TRUE)
#' getParamIds(ps, repeated = TRUE, with.nr = TRUE)
splitVectorParam = function(par) {
  assertClass(par, "Param")
  if (!isVector(par))
    stopf("Function can only be applied to vector params, you passed: '%s'", class(par)[1L])
  pids = getParamIds(par, repeated = TRUE, with.nr = TRUE)
  xs = lapply(pids, function(pid) {
    x = par
    x$type = gsub("vector", "", par$type)
    x$len = 1L
    return(x)
  })
  setNames(xs, pids)
}
