#' Plot method for optimization paths.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iters [\code{integer} | NULL]\cr
#'   Vector of iteration which should be plotted one after another. If \code{NULL},
#'   which is the default, all iterations are plotted. Iteration 0 plots
#'   all elements with dob = 0
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param alpha [\code{logical(1)}]\cr
#'   Activates or deactivates the alpha fading for the parallel X-space plot. Default is \code{TRUE}.
#' @return List of plots, one for each iteration.
#' @param lim.x [\code{list} | NULL]\cr
#'   Length of list equals dimensionality of X-Space. Not used, if the dimension
#'   is higher than 2. Each element is a \code{numeric(2)}, determining
#'   the x limits for the plots. Default is NULL, in this case min and max
#'   values of the data will be used.
#' @param lim.y [\code{list} | NULL]\cr
#'   Length of list equals dimensionality of X-Space. Not used, if the dimension
#'   is higher than 2. Each element is a \code{numeric(2)}, determining
#'   the y limits for the plots. Default is NULL, in this case min and max
#'   values of the data will be used.
#' @export
#' 
plotOptPath = function(op, iters, pause = TRUE, alpha = TRUE, lim.x = NULL, lim.y = NULL) {
  requirePackages("gridExtra", why = "plotOptPath")
  
  iters.max = max(getOptPathDOB(op))
  if (missing(iters)) {
    iters = 0:iters.max
  }
  
  assertIntegerish(iters, lower = 0L, upper = iters.max, any.missing = FALSE)
  assertFlag(pause)
  
  x.names = colnames(getOptPathX(op))
  y.names = op$y.names
  dim.x = length(x.names)
  dim.y = length(y.names)
  
  # Set and check x and y lims, if needed
  # consider only points alive during at least 1 plotted iteration
  if (dim.x <= 2) {
    op.x = as.data.frame(op, include.y = FALSE, include.rest = FALSE,
      dob = 0:max(iters), eol = c(min(iters):iters.max, NA)) 
    lim.x = getOptPathLims(lim.x, op.x, 0.05)
    
  }
  
  if (dim.y <= 2) {
    op.y = as.data.frame(op, include.x = FALSE, include.rest = FALSE,
      dob = 0:max(iters), eol = c(min(iters):iters.max, NA))  
    lim.y = getOptPathLims(lim.y, op.y, 0.05)
  }
  
  # Helper to arragne plot via gridExtra and pause process
  arrangePlots = function(plots) {
    plots = Filter(Negate(isScalarNA), plots)
    do.call(grid.arrange, c(plots, nrow = 1L))
    if (pause)
      pause()
  }
  
  for (iter in iters) {
    # get rendered plot data
    plots = renderOptPathPlot(op, iter = iter, lim.x = lim.x, lim.y = lim.y,
      alpha = alpha)
    arrangePlots(plots)
  }
  
}