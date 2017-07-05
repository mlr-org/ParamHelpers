readPCS = function(file) {
  consume = function(s, regexp) {
    loc = stri_locate_first_regex(s, regexp)[1L, ]
    e = substr(s, loc[1L], loc[2L])
    r = paste0(stri_sub(s, 1, loc[1L] - 1L), stri_sub(s, loc[2L] + 1L, stri_length(s)))
    list(match = stri_trim_both(e), rest = stri_trim_both(r))
  }

  parseDefault = function(s, convert = as.character) {
    s = consume(s, "\\[.*\\]")
    s = stri_trim_both(stri_replace_all_regex(s$match, "[\\[\\]]", ""))
    convert(s)
  }


  assertFileExists(file, access = "r", extension = "pcs")

  lines = readLines(file)
  lines = stri_replace_all_regex(lines, "#.*", "")
  lines = stri_trim_both(lines)
  lines = lines[nzchar(lines)]
  result = list()

  j = stri_detect_fixed(lines, "|")
  lines.cond = lines[j]
  lines = lines[!j]

  j = stri_startswith_fixed(lines, "{") & stri_endswith_fixed(lines, "}")
  lines.forbidden = lines[j]
  lines = lines[!j]

  # remove Conditionals start line
  lines = lines[!stri_startswith_fixed(lines, "Conditionals:")]
  # remove forbidden start line
  lines = lines[!stri_startswith_fixed(lines, "Forbidden:")]


  lines.params = lines

  ### parse param lines
  for (i in seq_along(lines.params)) {
    line = lines.params[i]
    # print(line)
    z = consume(line, "[a-zA-Z0-9_\\-]+\\s*")
    id = z$match

    if (stri_startswith_fixed(z$rest, "[")) { # num or int param
      z = consume(z$rest, "^\\[.*?\\]")
      bounds = stri_replace_all_regex(z$match, "[\\[\\]]", "")
      bounds = as.numeric(stri_trim_both(stri_split_fixed(bounds, ",")[[1L]]))
      if (stri_detect_fixed(z$rest, "i")) {
        def = parseDefault(z$rest, as.integer)
        par = makeIntegerParam(id = id, lower = bounds[1L], upper = bounds[2L], default = def)
      } else {
        def = parseDefault(z$rest, as.double)
        par = makeNumericParam(id = id, lower = bounds[1L], upper = bounds[2L], default = def)
      }
    } else if (stri_startswith_fixed(z$rest, "{")) {
      # discrete
      z = consume(z$rest, "^\\{.*\\}")
      values = stri_replace_all_regex(z$match, "[{}]", "")
      values = stri_trim_both(stri_split_fixed(values, ",")[[1L]])
      def = parseDefault(z$rest)
      par = makeDiscreteParam(id = id, values = values, default = def)
    } else {
      stop("Should not happen!")
    }
    result[[id]] = par
  }

  for (i in seq_along(lines.cond)) {
    line = lines.cond[i]
    s = stri_trim_both(stri_split_fixed(line, "|")[[1L]])
    id1 = s[1L]
    stopifnot(id %in% names(result))

    s = stri_trim_both(stri_split_fixed(s[2L], " in ")[[1L]])
    id2 = s[1L]
    stopifnot(id2 %in% names(result))

    # now take parse values and quote them
    vals = stri_trim_both(stri_split_fixed(stri_replace_all_regex(s[2L], "[{}]", ""), ",")[[1L]])
    req = sprintf("%s %%in%% c('%s')", id2, stri_flatten(vals, "','"))
    req = quote(req)
    result[[id1]]$requires = req
  }

  result
}
