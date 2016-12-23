#' @title Split up a vector param into a list of length-1 params.
#'
#' @description
#' Splits up vector params into a multiple length-1 params.
#' Sometimes a useful conversion if it is nicer to operate on
#' the individual normal params.
#'
#' @template arg_par_or_set
#' @return [list of \code{\link{Param}} | \code{\link{ParamSet}}].
#'   Return a list for single params and a (converted) param set for param sets.
#' @export
splitVectorParams = function(par) {
  UseMethod("splitVectorParams")
}

#' @export
splitVectorParams.Param = function(par) {
  pids = getParamIds(par, repeated = TRUE, with.nr = TRUE)
  xs = lapply(seq_along(pids), function(i) {
    pid = pids[i]
    x = par
    x$id = pid
    x$type = gsub("vector", "", par$type)
    x$len = 1L
    if (isNumeric(par)) {
      x$lower = par$lower[i]
      x$upper = par$upper[i]
    }
    if (isDiscrete(par)) {
      x$values = par$values
    }
    return(x)
  })
  setNames(xs, pids)
}

#' @export
splitVectorParams.ParamSet = function(par) {
  ps = lapply(par$pars, splitVectorParams.Param)
  ps = unlist(ps, recursive = FALSE)
  names(ps) = extractSubList(ps, "id")
  par$pars = ps
  return(par)
}

