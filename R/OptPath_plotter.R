#' Plot method for optimization paths.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iters [\code{integer} | NULL]\cr
#'   Vector of iterations which should be plotted one after another. If \code{NULL},
#'   which is the default, all iterations are plotted. Iteration 0 plots
#'   all elements with dob = 0.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @template arg_opplotter_lims
#' @param title [\code{character(1)}]\cr
#'   Main title for the arranged plots, default is Optimization Path Plots. 
#' @param ... 
#'   Additional parameters for \code{\link{renderOptPathPlot}}.
#' @return List of plots, one for each iteration.
#' @export
#' 
plotOptPath = function(op, iters, pause = TRUE, xlim = list(), ylim = list(), 
  title = "Optimization Path Plots", ...) {
  
  requirePackages("gridExtra", why = "plotOptPath")
  
  iters.max = max(getOptPathDOB(op))
  if (missing(iters))
    iters = 0:iters.max
  
  assertIntegerish(iters, lower = 0L, upper = iters.max, any.missing = FALSE)
  assertFlag(pause)
  assertCharacter(title, len = 1L)
  
  # Set and check x and y lims, if needed
  # Consider only points alive during at least 1 plotted iteration
  # Set and check x and y lims, if needed
  data = getAndSubsetPlotData(op, iters, ...)
  lims = getOptPathLims(xlim, ylim, data$op.x, data$op.y, iters, 0.05)
  xlim = lims$xlim
  ylim = lims$ylim
  
  # Helper to arrange plot via gridExtra and pause process
  arrangePlots = function(plots, iter, iters) {
    plots = Filter(Negate(isScalarNA), plots)
    do.call(gridExtra::grid.arrange, c(plots, nrow = 1L, main = title))
    if (pause && iter != getLast(iters)) {
      pause()
    }
  }
  
  for (iter in iters) {
    # get rendered plot data
    plots = renderOptPathPlot(op, iter = iter, xlim = xlim, ylim = ylim, ...)
    arrangePlots(plots, iter, iters)
  }
  
  return(invisible(NULL))
}
