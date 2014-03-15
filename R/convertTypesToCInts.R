# integer encoding of types for easier use
convertTypesToCInts = function(types) {
  lookup = 1:4
  nlookup = c("numeric", "integer", "factor", "logical")
  int.type = lookup[match(types, nlookup)]
  # FIXME: 99? wtf?
  int.type[is.na(int.type)] = 99L
  return(int.type)
}
