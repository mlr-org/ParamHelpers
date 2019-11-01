#' @title Plot method for optimization paths.
#'
#' @description Plot method for every type of optimization path, containing any
#' numbers and types of variables. For every iteration up to 4 types of plots
#' can be generated: One plot for the distribution of points in X and Y space
#' respectively and plots for the trend of specified X variables, Y variables
#' and extra measures over the time.
#'
#' @param op (`OptPath`)\cr
#'   Optimization path.
#' @param iters (`integer` | NULL)\cr
#'   Vector of iterations which should be plotted one after another. If `NULL`,
#'   which is the default, only the last iteration is plotted. Iteration 0 plots
#'   all elements with dob = 0. Note that the plots for iteration i contains all
#'   observations alive in iteration i.
#' @param pause (`logical(1)`)\cr
#'   Should the process be paused after each iteration?
#'   Default is `TRUE`.
#' @template arg_opplotter_lims
#' @param title (`character(1)`)\cr
#'   Main title for the arranged plots, default is Optimization Path Plots.
#' @param ...
#'   Additional parameters for [renderOptPathPlot()].
#' @return NULL
#' @export
#'
plotOptPath = function(op, iters, pause = TRUE, xlim = list(), ylim = list(),
  title = "Optimization Path Plots", ...) {

  requirePackages(c("grid", "gridExtra"), why = "plotOptPath")

  if (missing(iters)) {
    iters = max(getOptPathDOB(op))
  }

  assertClass(op, "OptPath")
  assertIntegerish(iters, lower = 0L, upper = max(getOptPathDOB(op)), any.missing = FALSE)
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

    # align plots
    plots = toGTable(plots)
    max.width = getMaxPlotWidth(plots)
    plots = toAlignedGTable(plots, max.width)

    if (!is.null(plots$plot.x.over.time)) {
      plots$plot.x.over.time = gridExtra::arrangeGrob(grobs = plots$plot.x.over.time, ncol = 1L)
    }

    if (!is.null(plots$plot.y.over.time)) {
      plots$plot.y.over.time = gridExtra::arrangeGrob(grobs = plots$plot.y.over.time, ncol = 1L)
    }

    plot.top = Filter(Negate(is.null), list(plots$plot.x, plots$plot.y))
    plot.top = gridExtra::arrangeGrob(grobs = plot.top, nrow = 1L)

    plot.bottom = Filter(Negate(is.null), list(plots$plot.x.over.time, plots$plot.y.over.time))

    if (length(plot.bottom) > 0L) {
      plot.bottom = do.call(gridExtra::arrangeGrob, c(plot.bottom, nrow = 1L))
      plots = list(plot.top, plot.bottom)
    } else {
      plots = list(plot.top)
    }

    gridExtra::grid.arrange(grobs = plots, ncol = 1L, main = title)
    if (pause && iter != getLast(iters)) {
      pause()
    }
  }

  # Get rendered data and plot it for every iteration
  for (iter in iters) {
    plots = renderOptPathPlot(op, iter = iter, xlim = xlim, ylim = ylim, ...)
    arrangePlots(plots, iter, iters)
  }

  return(invisible(NULL))
}

# Helper functions to ensure nice plot alignment.
#
# If plots are aligned in a grid via gridExtra::grid.arrange we are frequently
# faced with the ugly cosmetic problem, that the plot areas are unaligned if
# different plots have different y-axis scales.
#
# The following helper functions ensure nice alignment by doing three things:
# 1) transform ggplot objects to gtable objects.
# 2) extract the maximum left margin width of the gtables as a unit object.
# 3) Assign the maximum left margin width determined in 2) to all gtable
#
# All three functions operate on lists of lists of ggplot objects and can
# handle NULL objects.
#
# NOTE: the alignment procedure fails if there is at least one plot with facets!


# Transform ggplot objects to gtable objects.
#
# @param pls [list of (lists of) ggplot object(s)]
toGTable = function(pls) {
  lapply(pls, function(pl) {
    if (inherits(pl, "ggplot")) {
      return(ggplot2::ggplot_gtable(ggplot2::ggplot_build(pl)))
    }
    if (inherits(pl, "gtable") || is.null(pl)) {
      return(pl)
    }
    return(toGTable(pl))
  })
}

# Determine maximal left margin width of gtables recursively.
#
# @param pls [list of (lists of) gtable object(s)]
getMaxPlotWidth = function(pls) {
  do.call(grid::unit.pmax, lapply(pls, function(pl) {
    if (is.null(pl)) {
      return(pl)
    }
    if (!inherits(pl, "gtable")) {
      return(getMaxPlotWidth(pl))
    }
    return(pl$widths[2:3])
  }))
}

# Set left margin width of gtables recursively.
#
# @param pls [list of (lists of) gtable object(s)]
# @param pls [list of units]
toAlignedGTable = function(pls, max.width) {
  lapply(pls, function(pl) {
    if (is.null(pl)) {
      return(pl)
    }
    if (!inherits(pl, "gtable")) {
      return(toAlignedGTable(pl, max.width))
    }
    pl$widths[2:3] = max.width
    return(pl)
  })
}
