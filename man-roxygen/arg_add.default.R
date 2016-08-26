#' @param add.default [\code{logical(1)}]\cr
#'   Should one fixed design point be added at the default value of all parameters? If \code{TRUE}
#'   only \code{n-1} points are sampled and a design point at the default values is added as the first
#'   row to the result. This requires that all parameters in the param set actually have a default.
#'   Default is \code{FALSE}.