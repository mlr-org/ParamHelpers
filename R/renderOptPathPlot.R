#' Functions for plotting optimization paths.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iter [\code{integer(1)} | NULL]\cr
#'   Selected iteration of \code{x} to render plots for.
#' @param alpha [\code{logical(1)}]\cr
#'   Activates or deactivates the alpha fading for the parallel X-space plot. Default is \code{TRUE}.
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
#' @return List of plots. If both X and Y space are 1D, list has length 1,
#'   otherwise length 2 with one plot for X and Y space respectivly.
#' @export
renderOptPathPlot = function(op, iter, alpha = TRUE, lim.x = NULL, lim.y = NULL) {
  requirePackages("GGally", why = "renderOptPathPlot")
  requirePackages("ggplot2", why = "renderOptPathPlot")
  
  iters.max = max(getOptPathDOB(op))
  assertIntegerish(iter, len = 1L, lower = 0L, upper = iters.max, any.missing = FALSE)
  
  x.names = colnames(getOptPathX(op))
  y.names = op$y.names
  dim.x = length(x.names)
  dim.y = length(y.names)
  
  # consider only points alive at iteration iter
  op.x = as.data.frame(op, include.y = FALSE, include.rest = FALSE,
    dob = 0:iter, eol = c(iter:iters.max, NA))
  op.y = as.data.frame(op, include.x = FALSE, include.rest = FALSE,
    dob = 0:iter, eol = c(iter:iters.max, NA))
  dob = getOptPathDOB(op, dob = 0:iter, eol = c((iter + 1):iters.max, NA))
  
  # Set and check x and y lims, if needed
  if (dim.x <= 2) 
    lim.x = getOptPathLims(lim.x, op.x, 0.05)
  
  if (dim.y <= 2) 
    lim.y = getOptPathLims(lim.y, op.y, 0.05)
  
  # Set alpha and type values
  .alpha = if(alpha && iter > 0)
     normalize(dob, "range", range = c(1 / (iter + 1), 1)) else rep(1, length(dob))
  .type = as.factor(ifelse(dob == 0, "init", ifelse(dob == iter, "prop", "seq")))
  
  # Special case: X and Y are 1D
  if(dim.x == 1L && dim.y == 1L) {
    pl = plot2D(cbind(x = op.x, y = op.y), .alpha, .type,
      names = c(x.names, y.names), space = "both", iter = iter, lim = rbind(lim.x, lim.y))
    return(list(plot = pl))
  }
  
  # plot 1: x-space
  if (dim.x == 1) {
    pl1 = plot1D(op.x, .alpha, .type, x.names, space = "x", iter = iter,lim = lim.x)
  }
  if (dim.x == 2) {
    pl1 = plot2D(op.x, .alpha, .type, x.names, space = "x", iter = iter, lim = lim.x)
  } 
  if (dim.x > 2) {
    pl1 = plotMultiD(op.x, .alpha, .type, x.names, space = "x", iter = iter, alpha = alpha)
  }
  
  # plot 2: y-space
  if (dim.y == 1) {
    pl2 = plot1D(op.y, .alpha, .type, y.names, space = "y", iter = iter, lim = lim.y)
  }
  if (dim.y == 2) {
    pl2 = plot2D(op.y, .alpha, .type, y.names, space = "y", iter = iter, lim = lim.y)
  } 
  if (dim.y > 2) {
    pl2 = plotMultiD(op.y, .alpha, .type, y.names, space = "y", iter = iter, alpha = alpha)
  }
  
  return(list(plot.x = pl1, plot.y = pl2))
}


# Plot method for a one-dimensional X- or Y-Space
#
# @param .alpha [\code{numeric}]\cr
#   Vector of alpha values for the points in the plots
# @param .type [\code{factor}]\cr
#   Vector of types of the points, factor levels are init, seq and prob.
# @param names [\code{character}]\cr
#   Vector of the names of the variables.
# @param space[\code{character}]
#   If the X-Space is plotted, space = "x" and if the Y-Space is plotted, space = "y".
# @param iter [\code{integer(1)}]\cr
#   Current iteration.
# @param lim [\code{matrix}]\cr
#  Matrix with 1 row and 2 cols. Axis limits for the density plot.
# @return A ggplot object.
plot1D = function(op, .alpha, .type, names, space, iter, alpha = TRUE, lim) {
  op$.alpha = .alpha
  op$type = .type
  
  if (space == "x") {  
    title = ggplot2::ggtitle("X-Space")    
  } 
  if (space == "y") {    
    title = ggplot2::ggtitle("Y-Space")    
  }
  
  pl = ggplot2::ggplot(op, ggplot2::aes_string(x = names[1]))
  pl = pl + ggplot2::geom_density(colour = "black")
  pl = pl + title
  pl = pl + ggplot2::geom_rug(ggplot2::aes_string(colour = "type", alpha = ".alpha"), sides = "b", size = 2,
    data = op)
  pl = pl + ggplot2::xlim(lim[[1]])
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_colour_manual(values = c("red","green", "blue"))
  pl = pl + ggplot2::scale_alpha_continuous(range = c(1 / (iter + 1), 1))
  pl = pl + ggplot2::theme(legend.position = "top")
return(pl)
}

# Plot method for a two-dimensional X- or Y-Space
#
# @param lim [\code{matrix}]\cr
#  Matrix with two columns. Each row contains the lower and upper limit for the 
#  x- and y- axes of the two-dimensional plot.
# @return A ggplot object.
plot2D = function(op, .alpha, .type, names, space, iter, alpha = TRUE, lim) {
  op$.alpha = .alpha
  op$type = .type
  
  if (space == "x") {
    title = ggplot2::ggtitle("X-Space")
    x.lab = ggplot2::xlab(expression(x[1]))
    y.lab = ggplot2::ylab(expression(x[2]))
  } 
  if (space == "y") {
    title = ggplot2::ggtitle("Y-Space")
    x.lab = ggplot2::xlab(expression(y[1]))
    y.lab = ggplot2::ylab(expression(y[2]))
  }
  if (space == "both") {
    title = ggplot2::ggtitle("X- and Y-Space")
    x.lab = ggplot2::xlab(expression(x[1]))
    y.lab = ggplot2::ylab(expression(y[1]))
  }
  
  pl = ggplot2::ggplot(op, ggplot2::aes_string(
    x = names[1], y = names[2], shape = "type", colour = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_point(size = 3)
  pl = pl + title
  pl = pl + x.lab + y.lab
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::xlim(lim[[1]]) + ggplot2::ylim(lim[[2]])
  pl = pl + ggplot2::scale_colour_manual(values = c("red","green", "blue"))
  pl = pl + ggplot2::scale_alpha_continuous(range = c(1 / (iter + 1), 1))
  pl = pl + ggplot2::theme(legend.position = "top")
  return(pl)
}

# Plot method for a multi-dimensional X- or Y-Space
# @return A ggplot object.
plotMultiD = function(op, .alpha, .type, names, space = "x", iter, alpha = TRUE) {
  args = list(columns = seq_along(names))
  op$.alpha = .alpha
  op$type = .type
  args$data = op
  args$alphaLines = ".alpha"
  args$groupColumn = ncol(op)
  args$mapping = ggplot2::aes(lwd = 1.5)
  
  if (space == "x") {
    title = ggplot2::ggtitle("X-Space")
  } else {
    title = ggplot2::ggtitle("Y-Space")
  }
  
  pl = do.call(GGally::ggparcoord, args)
  pl = pl + ggplot2::ylab ("value divided by standard deviation")
  pl = pl + title
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_colour_manual(values = c("red","green", "blue"))
  pl = pl + ggplot2::theme(legend.position = "top", legend.margin = grid::unit(0.05, "cm"))
  return(pl)
}