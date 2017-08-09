#' @title Writes a param set to a PCS files.
#'
#' @description
#'
#' @template arg_parset
#' @param path [\code{character(1)}]\cr
#'   File path for output PCS file.
#' @param overwrite [\code{logical(1)}]\cr
#'   Can \code{path} be overwritten?
#'   Default is \code{FALSE}.
#' @return Nothing.
#' @family ParamSet, pcs
writePCSFile = function(par.set, path, overwrite = FALSE) {
  assertClass(par.set, "ParamSet")
  assertPathForOutput(path, overwrite = overwrite)
  assertFlag(overwrite)
  lines = character(0L) # result container

  # write line for a scalar (nonvec) param, returns string
  writePCSScalarLine = function(p) {
    s = sprintf("%s [%g,%g] [%g]", p$id, p$lower, p$upper, p$default)
    # print(s)
    return(s)
  }

  # loop over params, loop over length of vectors, add line to "lines"
  for (p in par.set$pars) {
    len = p$len
    for (j in 1:p$len) {
      lines[length(lines) + 1L] = writePCSScalarLine(p)
    }
  }
  stri_write_lines(fname = path, str = lines)
  invisible(NULL)
}
