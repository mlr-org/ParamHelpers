#' @title Converts a ParamSet object to a parameter object of the irace package.
#'
#' @description
#' Converts to a textual description used in irace and then potentially calls \link[irace]{readParameters}.
#'
#' @template arg_parset
#' @param as.chars [\code{logical(1)}]\cr
#'   Return results as character vector of lines \code{FALSE} or call
#'   \code{\link[irace]{readParameters}} on it (\code{TRUE}).
#'   Default is \code{FALSE}.
#' @return [\code{\link{list}}].
#' @export
convertParamSetToIrace = function(par.set, as.chars = FALSE) {
  assertClass(par.set, "ParamSet")
  assertFlag(as.chars)
  if (!is.null(par.set$forbidden))
    stopf("Operation not allowed for param set with forbidden region currently!")
  if (!hasFiniteBoxConstraints(par.set))
    stop("convertParamSetToIrace requires finite box constraints for all numeric and integer params!")
  requirePackages("irace", why = "convertParamSetToIrace", default.method = "load")
  lines = character(0L)
  count = 1L
  for (i in seq_along(par.set$pars)) {
    p = par.set$pars[[i]]
    type = switch(
      p$type,
      numeric = "r",
      numericvector = "r",
      integer = "i",
      integervector = "i",
      discrete = "c",
      discretevector = "c",
      logical = "c",
      logicalvector = "c",
      ordered = "o"
    )
    for (j in seq_len(p$len)) {
      id = if (p$len == 1L) p$id else paste(p$id, j, sep = "")
      if (p$type %in% c("numeric", "numericvector"))
        line = sprintf('%s "" %s (%g, %g)', id, type, p$lower[j], p$upper[j])
      else if (p$type %in% c("integer", "integervector"))
        line = sprintf('%s "" %s (%i, %i)', id, type, p$lower[j], p$upper[j])
      else if (p$type %in% c("discrete", "discretevector", "logical", "logicalvector")) {
        v = paste("\"", names(p$values), "\"", sep = "")
        line = sprintf('%s "" %s (%s)', id, type, collapse(v))
      } else  {
        stopf("Unknown parameter type: %s", p$type)
      }
      if (!is.null(p$requires)) {
        line = paste(line, collapse(deparse(p$requires, width.cutoff = 500L), sep=""), sep = " | ")
      }
      lines[count] = line
      count = count + 1L
    }
  }
  if (as.chars) {
    return(lines)
  } else {
    lines = collapse(lines, "\n")
    params = irace::readParameters(text = lines, digits = .Machine$integer.max)
    # fix numeric boundaries of irace, to make sure we dont lose num accuracy by write/read file IO
    # somehow bad....
    for (p in par.set$pars) {
      if (isNumeric(p, include.int = TRUE)) {
        pids = getParamIds(p, repeated = TRUE, with.nr = TRUE)
        for (j in seq_len(p$len)) {
          if (isNumericStrict(p))
            params$boundary[[pids[j]]] = c(p$lower[j], p$upper[j])
          if (isInteger(p))
            params$boundary[[pids[j]]] = as.integer(c(p$lower[j], p$upper[j]))
        }
      }
    }
    return(params)
  }
}


