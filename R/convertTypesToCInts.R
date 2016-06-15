# integer encoding of types for easier use
convertTypesToCInts = function(types) {
  nlookup = c("numeric", "integer", "factor", "logical", "character")
  match(types, nlookup, nomatch = 99L)
}
