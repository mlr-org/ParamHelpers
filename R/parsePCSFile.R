consume = function(s, regexp) {
  loc = stri_locate_first_regex(s, regexp)[1L, ]
  e = stri_sub(s, loc[1L], loc[2L])
  r = stri_join(stri_sub(s, 1L, loc[1L] - 1L), stri_sub(s, loc[2L] + 1L, stri_length(s)))
  list(match = stri_trim_both(e), rest = stri_trim_both(r))
}

parseDefault = function(s) {
  s = consume(s, "\\[.*\\]")
  stri_trim_both(stri_replace_all_regex(s$match, "[\\[\\]]", ""))
}

stri_split_trim = function(x, sep = ",") {
  stri_trim_both(stri_split_fixed(x, sep)[[1L]])
}

#' @title Read and parse PCS files
#'
#' @param file [\code{character(1)}].\cr
#'   Path to the PCS file.
#' @return \code{\link{ParamSet}}.
#' @export
parsePCSFile = function(file) {
  assertFileExists(file, access = "r", extension = "pcs")

  lines = stri_read_lines(file)
  lines = stri_replace_all_regex(lines, "#.*", "")
  lines = stri_trim_both(lines)
  lines = lines[nzchar(lines)]
  result = list()

  lines = lines[!stri_startswith_fixed(lines, "Conditionals:")]
  j = stri_detect_fixed(lines, "|")
  lines.cond = lines[j]
  lines = lines[!j]

  lines = lines[!stri_startswith_fixed(lines, "Forbidden:")]
  j = stri_startswith_fixed(lines, "{") & stri_endswith_fixed(lines, "}")
  lines.forbidden = lines[j]
  lines = lines[!j]

  ### parse param lines
  for (line in lines) {
    z = consume(line, "[a-zA-Z0-9_\\-]+\\s*")
    id = z$match

    if (stri_startswith_fixed(z$rest, "[")) { # num or int param
      z = consume(z$rest, "^\\[.*?\\]")
      bounds = stri_replace_all_regex(z$match, "[\\[\\]]", "")
      bounds = as.numeric(stri_split_trim(bounds))
      if (stri_detect_fixed(z$rest, "i")) {
        def = as.numeric(parseDefault(z$rest))
        par = makeIntegerParam(id = id, lower = bounds[1L], upper = bounds[2L], default = def)
      } else {
        def = as.integer(parseDefault(z$rest))
        par = makeNumericParam(id = id, lower = bounds[1L], upper = bounds[2L], default = def)
      }
    } else if (stri_startswith_fixed(z$rest, "{")) { # discrete
      z = consume(z$rest, "^\\{.*\\}")
      values = stri_replace_all_regex(z$match, "[{}]", "")
      values = stri_split_trim(values)
      def = parseDefault(z$rest)
      par = makeDiscreteParam(id = id, values = values, default = def)
    } else {
      stop("Illegal format")
    }
    result[[id]] = par
  }

  for (line in lines.cond) {
    s = stri_split_trim(line, "|")
    id1 = s[1L]
    stopifnot(id %in% names(result))

    s = stri_split_trim(s[2L], " in ")
    id2 = s[1L]
    stopifnot(id2 %in% names(result))

    vals = stri_split_trim(stri_replace_all_regex(s[2L], "[{}]", ""))
    req = sprintf("%s %%in%% c('%s')", id2, stri_flatten(vals, "','"))
    req = quote(req)
    result[[id1]]$requires = req
  }

  result
}
