#' Plot method for optimization paths.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iters [\code{numeric} | NULL]\cr
#'   Vector of iteration which should be plotted one after another. If \code{NULL},
#'   which is the default, all iterations are plotted.
#' @param alpha [\code{logical(1)}]\cr
#'   Activates or deactivates the alpha fading for the parallel X-space plot. Default is \code{TRUE}.
#' @return List of plots, one for each iteration.
#' @export
renderOptPathPlot = function(op, iters, alpha = TRUE) {
  requirePackages("GGally")
  requirePackages("ggplot2")
  requirePackages("gridExtra")
  
  if (is.null(iters))
    iters = unique(getOptPathDOB(op))
  assertIntegerish(iters)
  
  if (any(iters > max(getOptPathDOB(op))))
    stop("You want to plot an iteration that does not exist!")
  
  # FIXME: better way to get x-names?
  x.names = colnames(getOptPathX(op))
  y.names = op$y.names
  dim.x = length(x.names)
  dim.y = length(y.names)
  
  op.x = as.data.frame(op, include.y = FALSE, include.rest = FALSE)
  op.y = as.data.frame(op, include.x = FALSE, include.rest = FALSE)
  dob = getOptPathDOB(op)
  
  if (dim.x == 2) {
    x.min = apply(op.x, 2, min)
    x.max = apply(op.x, 2, max)
    x.min = x.min - 0.05 * (x.max - x.min)
    x.max = x.max + 0.05 * (x.max - x.min)
    lim.x = cbind(x.min, x.max)
  }
  if (dim.y == 2) {
    y.min = apply(op.y, 2, min)
    y.max = apply(op.y, 2, max)
    y.min = y.min - 0.05 * (y.max - y.min)
    y.max = y.max + 0.05 * (y.max - y.min)
    lim.y = cbind(y.min, y.max)
  }
  
  plots = list()
  for (i in seq_along(iters)) {
    # get the data
    op.x.temp = op.x[dob <= iters[i], ,drop = FALSE]
    op.y.temp = op.y[dob <= iters[i], ,drop = FALSE]
    dob.temp = dob[dob <= iters[i]]
    
    # plot 1: x-space
    if (dim.x == 1) {
      pl1 = plot1D(op.x.temp, dob.temp, x.names, space = "x", iter = iters[i])
    }
    if (dim.x == 2) {
      pl1 = plot2D(op.x.temp, dob.temp, x.names, space = "x", iter = iters[i], lim = lim.x)
    } 
    if (dim.x > 2) {
      pl1 = plotMultiD(op.x.temp, dob.temp, x.names, space = "x", iter = iters[i], alpha = alpha)
    }
    
    # plot 2: y-space
    if (dim.y == 1) {
      pl2 = plot1D(op.y.temp, dob.temp, y.names, space = "y", iter = iters[i])
    }
    if (dim.y == 2) {
      pl2 = plot2D(op.y.temp, dob.temp, y.names, space = "y", iter = iters[i], lim = lim.y)
    } 
    if (dim.y > 2) {
      pl2 = plotMultiD(op.y.temp, dob.temp, y.names, space = "y", iter = iters[i], alpha = alpha)
    }
    
    plots[[i]] = grid.arrange(pl1, pl2, nrow = 1, heights = c(2, 1))
  }
  return(plots)
}


#' Plot method for a one-dimensional X- or Y-Space
#'
#' @param dob [\code{numeric}]\cr
#'   Vector of dates of birth of the points that should be plotted.
#' @param names [\code{character}]\cr
#'   Vector of the names of the variables.
#' @param space [\code{character(1)}]\cr
#'   If the X-Space is plotted, space = "x" and if the Y-Space is plotted, space = "y".
#' @param iter [\code{integer(1)}]\cr
#'   Current iteration.
#' @return A ggplot object.
plot1D = function(op, dob, names, space = "x", iter, alpha = TRUE) {
  if (alpha) {
    op$.alpha = if(iter == 0)
      rep(1, length(dob)) else normalize(dob, "range", range = c(1 / (iter + 1), 1))
  }
  op$type = as.factor(ifelse(dob == 0, "init", ifelse(dob == iter, "prop", "seq")))
  if (space == "x") {
    title = ggtitle("X-Space")
  } else {
    title = ggtitle("Y-Space")
  }
    
  pl = ggplot(op, aes_string(x = names[1]))
  pl = pl + geom_density(colour = "black")
  pl = pl + title
  pl = pl + geom_rug(aes(colour = type, alpha = .alpha), sides = "b", size = 2,
    data = op)
  pl = pl + guides(alpha = FALSE)
  pl = pl + scale_colour_manual(values = c("red","green", "blue"))
  pl = pl + scale_alpha_continuous(range = c(1 / (iter + 1), 1))
  pl = pl + theme(legend.position = "top")
return(pl)
}

#' Plot method for a two-dimensional X- or Y-Space
#'
#' @param lim [\code{matrix}]\cr
#'  Matrix with two columns. Each row contains the lower and upper limit for the 
#'  x- and y- axes of the two-dimensional plot.
#' @return A ggplot object.
plot2D = function(op, dob, names, space = "x", iter, lim) {
  op$type = as.factor(ifelse(dob == 0, "init", ifelse(dob == iter, "prop", "seq")))
  if (space == "x") {
    title = ggtitle("X-Space")
    x.lab = xlab(expression(x[1]))
    y.lab = ylab(expression(x[2]))
  } else {
    title = ggtitle("Y-Space")
    x.lab = xlab(expression(y[1]))
    y.lab = ylab(expression(y[2]))
  }
  
  pl = ggplot(op, aes_string(x = names[1], y = names[2], shape = "type", colour = "type"))
  pl = pl + geom_point(size = 3)
  pl = pl + title
  pl = pl + x.lab + y.lab
  pl = pl + xlim(lim[1, ]) + ylim(lim[2,])
  pl = pl + scale_colour_manual(values = c("red","green", "blue"))
  pl = pl + theme(legend.position = "top")
  return(pl)
}

#' Plot method for a multi-dimensional X- or Y-Space
#' @return A ggplot object.
plotMultiD = function(op, dob, names, space = "x", iter, alpha = TRUE) {
  args = list(columns = seq_along(names))
  if (alpha) {
    op$.alpha = if(iter == 0)
      rep(1, length(dob)) else normalize(dob, "range", range = c(1 / (iter + 1), 1))
    args$alphaLines = ".alpha"
  }
  op$type = as.factor(ifelse(dob == 0, "init", ifelse(dob == iter, "prop", "seq")))
  args$data = op
  args$groupColumn = ncol(op)
  args$mapping = aes(lwd = 1.5)
  
  if (space == "x") {
    title = ggtitle("X-Space")
  } else {
    title = ggtitle("Y-Space")
  }
  
  pl = do.call(GGally::ggparcoord, args)
  pl = pl + ylab ("value divided by standard deviation")
  pl = pl + title
  pl = pl + guides(alpha = FALSE)
  pl = pl + scale_colour_manual(values = c("red","green", "blue"))
  pl = pl + theme(legend.position = "top", legend.margin = grid::unit(0.05, "cm"))
  return(pl)
}