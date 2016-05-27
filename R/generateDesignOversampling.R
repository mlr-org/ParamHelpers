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
#' @param start.design [\code{data.frame}]
#'   The start design will be extended until size \code{n} is reached.
#'   The first rows of the generated design will match the start.design.
#'   Default is an empty data.frame.
#' @template ret_gendes_df
# Don't export yet!
# @export

generateDesignOversampling = function(
  n = 10L, par.set, trafo = FALSE, oversampling = 20L,
  # FIXME: is there a better way to create the empty data.frame?
  # makeDataFrame in BBmisc does not work with factors ...
  start.design = generateDesign(1, par.set)[NULL, ]) {
  
  requirePackages("cluster", why = "generateDesignOversampling")
  
  n = asCount(n)
  doBasicGenDesignChecks(par.set)
  assertNumber(oversampling, lower = 1L)
  assertLogical(trafo, len = 1L)
  assertDataFrame(start.design, types = getParamTypes(par.set, df.cols = TRUE),
    ncols = sum(getParamLengths(par.set)))
  
  # FIXME: Do we need a helper here?
  # I want to check that every row of a design is a feasible param.
  feas = sapply(
    convertRowsToList(start.design, name.list = TRUE, name.vector = TRUE),
    isFeasible, par = par.set)
  if (!all(feas)) {
    stop("One row of the start.design is not feasible.")
  }

  # Sample random Design with to many observations
  design = generateRandomDesign(n * oversampling, par.set, trafo)
  
  # Add start.desing
  design = rbind(start.design, design)
  
  # Calculate dist matrix. I know, we double the memory requirement here, 
  # but working with a matrix is much much easier ... we probably want to do
  # this in C on the long hand!
  dists = cluster::daisy(x = design, metric = "gower")
  dists = as.matrix(dists)
  # diag = Inf, not 0
  diag(dists) = Inf
  # start.design points are fixed
  dists[seq_row(start.design), seq_row(start.design)] = Inf
  
  # Shrink the design to n observations
  while (nrow(design) > n) {
    # Which observations should be removed? Sample between the candidates
    remove.ind = which(apply(dists == min(dists), 1, any))
    # Never remove start.design points
    remove.ind = remove.ind[remove.ind > nrow(start.design)]
    # If multiple candidates, sample
    if (length(remove.ind) > 1)
      remove.ind = sample(remove.ind, 1)
    # Remove it
    design = design[-remove.ind, ]
    dists = dists[-remove.ind, -remove.ind]
  }
  
  return(design)
}
