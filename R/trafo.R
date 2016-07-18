#' @title Transform a value.
#'
#' @description
#' Transform a value with associated transformation function(s).
#'
#' @template arg_par_or_set
#' @param x [any] \cr
#'   Single value to check.
#'   For a parameter set this must be a list. If the list is unnamed (not recommended) it must be in
#'   the same order as the param set. If it is named, its names must match the parameter names in the
#'   param set.
#' @return Transformed value.
#' @export
#' @examples
#' # transform simple parameter:
#' p = makeNumericParam(id="x", trafo=function(x) x^2)
#' trafoValue(p, 2)
#' # for a parameter set different transformation functions are possible:
#' ps = makeParamSet(
#'   makeIntegerParam("u", trafo=function(x) 2*x),
#'   makeNumericVectorParam("v", len=2, trafo=function(x) x/sum(x)),
#'   makeDiscreteParam("w", values=c("a", "b"))
#' )
#' # now the values of "u" and "v" are transformed:
#' trafoValue(ps, list(3, c(2, 4), "a"))
trafoValue = function(par, x) {
  if (inherits(par, "ParamSet")) {
    assertList(x, len = getParamNr(par))
    pids = getParamIds(par)
    if (is.null(names(x))) {
      # if we dont have names, assume list is in correct order and matches parset
      names(x) = pids
    } else {
      # if we have names, they must match the names in parset, and we reorder to be "safe"
      # and an later rely on the order
      assertSetEqual(names(x), pids)
      x = x[pids]
    }
    Map(trafoValue, par$pars, x)
  } else {
    if (is.null(par$trafo))
      x
    else
      par$trafo(x)
  }
}

#' Transform optimization path.
#'
#' Transform optimization path with associated transformation functions of parameters.
#' Can only be done when x values where added \dQuote{untransformed}.
#'
#' @param opt.path [\code{\link{OptPath}}]\cr
#'   Optimization path.
#' @return [\code{\link{OptPath}}].
#' @export
#' @examples
#' ps = makeParamSet(
#'   makeIntegerParam("u", trafo=function(x) 2*x),
#'   makeNumericVectorParam("v", len=2, trafo=function(x) x/sum(x)),
#'   makeDiscreteParam("w", values=c("a", "b"))
#' )
#' op = makeOptPathDF(ps, y.names="y", minimize=TRUE)
#' addOptPathEl(op, x=list(3, c(2, 4), "a"), y=0, dob=1, eol=1)
#' addOptPathEl(op, x=list(4, c(5, 3), "b"), y=2, dob=5, eol=7)
#'
#' as.data.frame(op)
#' op = trafoOptPath(op)
#' as.data.frame(op)
trafoOptPath = function(opt.path) {
  assertClass(opt.path, "OptPath")
  if (opt.path$add.transformed.x)
    stop("Cannot further trafo opt.path, you already added transformed x values to it!")
  ps = opt.path$par.set
  # FIXME: this only works for the DF implementation!
  op2 = makeOptPathDF(opt.path$par.set, opt.path$y.names, opt.path$minimize, add.transformed.x = TRUE,
    include.error.message = !is.null(opt.path$env$error.message),
    include.exec.time = !is.null(opt.path$env$exec.time),
    include.extra = !is.null(opt.path$env$extra))
  lapply(1:getOptPathLength(opt.path), function(i) {
    z = getOptPathEl(opt.path, i)
    x = trafoValue(ps, z$x)
    args = list(op = op2, x = x, y = z$y, dob = z$dob, eol = z$eol)
    # the next components can possibly be NULL, will be only added to arg list, if they are "there"
    args$error.message = z$error.message
    args$exec.time = z$exec.time
    args$extra = z$extra
    do.call(addOptPathEl, args)
  })
  return(op2)
}
