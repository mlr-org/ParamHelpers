#' @title Convert a list to a data.frame with one row
#'
#' @description
#' Convert a list of vectors or scalars to a `data.frame` with only one row. Names of the columns correspond to
#' the names of elements in the list. If a vector is one list element it is spread over multiple
#' columns and named sequentially, e.g. `a = c(5,7)` becomes `data.frame(a1 = 5, a2 = 7)`.
#'
#' @param l (`list`)\cr
#'  of atomic values of vectors.
#' @return (`data.frame`) with only one row, containing the list elements.
listToDfOneRow = function(l) {
  assertList(l, min.len = 1)
  lapply(l, assert_atomic_vector)
  l = lapply(seq_along(l), function(x) t(unlist(l[x])))
  data.frame(l)
}
