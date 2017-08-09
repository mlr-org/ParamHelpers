filterEmptyStrings = function(xs) {
  xs = vcapply(xs, stri_trim)
  xs[nzchar(xs)]
}
