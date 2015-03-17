#' Plot method for optimization paths.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iters [\code{integer} | NULL]\cr
#'   Vector of iteration which should be plotted one after another. If \code{NULL},
#'   which is the default, all iterations are plotted. Iteration 0 plots
#'   all elements with dob = 0.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param alpha [\code{logical(1)}]\cr
#'   Activates or deactivates the alpha fading for the plots. Default is \code{TRUE}.
#' @param lim.x [\code{list}], @param lim.y [\code{list}]\cr
#'   Axis limits for the plots. Must be a named list, so you can specify the
#'   axis limits for every plot. Every element of the list must be a numeric
#'   vector of length 2. Available names for elements are:
#'   XSpace - limits for the X-Space plot
#'   YSpace - limits for the Y-Space plot
#'   Default is an empty list - in this case limits are automatically set. 
#' @return List of plots, one for each iteration.
#' @export
#' 
plotOptPath = function(op, iters, pause = TRUE, alpha = TRUE, lim.x = list(), lim.y = list()) {
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
  # Set and check x and y lims, if needed
  tmp = getOptPathLims(lim.x, lim.y, op, iters, 0.05)
  lim.x = tmp$lim.x
  lim.y = tmp$lim.y
  
  # Helper to arragne plot via gridExtra and pause process
  arrangePlots = function(plots) {
    plots = Filter(Negate(isScalarNA), plots)
    do.call(gridExtra::grid.arrange, c(plots, nrow = 1L))
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