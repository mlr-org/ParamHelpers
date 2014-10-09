#' Converts a ParamSet object to a parameter object of the irace package.
#'
#' Converts to a textual description used in irace and then potentially calls \link[irace]{readParameters}.
#'
#' @template arg_parset
#' @param digits [\code{integer(1)}]\cr
#'   Number of digits used in transformation to irace textual description,
#'   code\link{formatC} is used here internally.
#' @param as.chars [\code{logical(1)}]\cr
#'   Return results as character vector of lines \code{FALSE} or call
#'   \code{\link[irace]{readParameters}} on it (\code{TRUE}).
#'   Default is \code{FALSE}.
#' @return [\code{\link{list}}].
#' @export
convertParamSetToIrace = function(par.set, digits = 4, as.chars = FALSE) {
  assertClass(par.set, "ParamSet")
  digits = asInt(digits, lower = 1L)
  assertFlag(as.chars)
  if (!is.null(par.set$forbidden))
    stopf("Operation not allowed for param set with forbidden region currently!")
  requirePackages("irace", "convertParamSetToIrace")
  lines = character(0)
  fnum = function(x) formatC(x, format = "f", digits = digits)
  count = 1
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
    for (j in 1:p$len) {
      id = if(p$len == 1) p$id else paste(p$id, j, sep = "")
      if (p$type %in% c("numeric", "numericvector"))
        line = sprintf('%s "" %s (%s, %s)', id, type, fnum(p$lower[j]), fnum(p$upper[j]))
      else if (p$type %in% c("integer", "integervector"))
        line = sprintf('%s "" %s (%i, %i)', id, type, p$lower[j], p$upper[j])
      else if (p$type %in% c("discrete", "discretevector", "logical", "logicalvector")) {
        v = paste("\"", names(p$values), "\"", sep = "")
        line = sprintf('%s "" %s (%s)', id, type, collapse(v))
      } else  {
        stopf("Unknown parameter type: %s", p$type)
      }
      if (!is.null(p$requires)) {
        line = paste(line, capture.output(p$requires), sep = " | ")
      }
      lines[count] = line
      count = count + 1
    }
  }
  if (as.chars) {
    return(lines)
  } else {
    lines = collapse(lines, "\n")
    return(irace::readParameters(text = lines))
  }
}
