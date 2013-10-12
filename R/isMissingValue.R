isMissingValue = function(x) {
  is.vector(x) && length(x) == 1L && is.na(x)
}


isMissingName = function(x) {
  identical(x, NA_character_)
}