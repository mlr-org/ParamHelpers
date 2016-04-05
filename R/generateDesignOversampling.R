#' @title Generates a random design for a mixed parameter set using oversampling
#'        and removing "close" design points. 
#'
#' @description
#' The function simply calls \code{\link{generateRandomDesign}} generating
#' \code{oversampling} more points than desired. Afterwards, repeatedly one of
#' the 2 points with the smallest Gower distance is removed until the design
#' is reduced to size \code{n}.
#'
#' @template arg_gendes_n
#' @template arg_parset
#' @template arg_trafo
#' @param oversampling [\code{double}]\cr
#'   Oversampling ratio. Must be greater than 1.
#' @template ret_gendes_df
# Don't export yet!
# @export

generateDesignOversampling = function(
  n = 10L, par.set, trafo = FALSE, oversampling = 20L) {
  
  requirePackages("cluster", why = "generateDesignOversampling")
  
  n = asCount(n)
  doBasicGenDesignChecks(par.set)
  assertNumber(oversampling, lower = 1L)
  assertLogical(trafo, len = 1L)
  
  # Sample random Design with to many observations
  design = generateRandomDesign(n * oversampling, par.set, trafo)
  
  # Calculate dist matrix. I know, we double the memory requirement here, 
  # but working with a matrix is much much easier ... we probably want to do
  # this in C on the long hand!
  dists = cluster::daisy(x = design, metric = "gower")
  dists = as.matrix(dists)
  diag(dists) = Inf
  
  # Shrink the design to n observations
  while (nrow(design) > n) {
    # Which observations should be removed? Sample between the candidates
    remove.ind = sample(which(apply(dists == min(dists), 1, any)), 1)
    # Remove it
    design = design[-remove.ind, ]
    dists = dists[-remove.ind, -remove.ind]
  }
  
  return(design)
}
