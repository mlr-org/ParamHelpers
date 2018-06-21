# integer encoding of types for easier use
convertTypesToCInts = function(types) {
  fmatch(types, ph$convert.to.ctypes, nomatch = 99L)
}
