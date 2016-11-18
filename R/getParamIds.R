#' @title Return ids of parameters in parameter set.
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
getParamIds = function(par, repeated = FALSE, with.nr = FALSE) {
  assertFlag(repeated)
  assertFlag(with.nr)
  UseMethod("getParamIds")
}

#' @export
getParamIds.ParamSet = function(par, repeated = FALSE, with.nr = FALSE) {
  if (isEmpty(par))
    return(character(0L))
  unlist(lapply(par$pars, getParamIds, repeated = repeated, with.nr = with.nr), use.names = FALSE)
}

#' @export
getParamIds.Param = function(par, repeated = FALSE, with.nr = FALSE) {
  pid = par$id
  if (repeated && isVector(par)) {
    n = par$len
    if (!is.na(n)) {
      if (n > 1L && with.nr)
        paste(rep(pid, n), seq_len(n), sep = "")
      else
        rep(pid, n)
    } else {
      pid
    }
  } else {
    pid
  }
}
