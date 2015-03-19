#' Functions for plotting optimization paths.
#'
#' @param op [\code{OptPath}]\cr
#'   Optimization path.
#' @param iter [\code{integer(1)} | NULL]\cr
#'   Selected iteration of \code{x} to render plots for.
#' @param lim.x [\code{list}], @param lim.y [\code{list}]\cr
#'   Axis limits for the plots. Must be a named list, so you can specify the
#'   axis limits for every plot. Every element of the list must be a numeric
#'   vector of length 2. Available names for elements are:
#'   XSpace - limits for the X-Space plot
#'   YSpace - limits for the Y-Space plot
#'   Default is an empty list - in this case limits are automatically set.  
#' @param alpha [\code{logical(1)}]\cr
#'   Activates or deactivates the alpha fading for the plots. Default is \code{TRUE}.
#' @param colours [\code{character(3)}]\cr
#'   Colours of the points/lines for the three point types init, seq and prob.
#' @param size [\code{numeric(2)} | NULL]\cr
#'   Size of points or lines for X and Y space. In the 1D-1D case only the
#'   first entry of \code{size} is used. If \code{NULL} \code{size = 3} for points and
#'   \code{lwd = 1.5} for lines is used.
#' @param impute.scale [\code{numeric(1)}]\cr
#'   Numeric missing values will be replaced by \code{max + impute.scale * (max - min)}.
#' @param impute.value [\code{character(1)}]\cr
#'   Factor missing values will be replaced by \code{impute.value}.    
#' @return List of plots. If both X and Y space are 1D, list has length 1,
#'   otherwise length 2 with one plot for X and Y space respectivly.
#' @export
renderOptPathPlot = function(op, iter, lim.x = list(), lim.y = list(), alpha = TRUE, 
    colours = c("red", "blue", "green"), size = NULL, impute.scale = 1, 
    impute.value = "missing") {
  
  requirePackages("GGally", why = "renderOptPathPlot")
  requirePackages("ggplot2", why = "renderOptPathPlot")
  
  iters.max = max(getOptPathDOB(op))
  assertIntegerish(iter, len = 1L, lower = 0L, upper = iters.max, any.missing = FALSE)
  
  # FIXME: Is there any better way to get these 2 informations?
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
  
  # impute missing values
  for (i in seq_len(ncol(op.x))) {
    op.x[, i] = imputeMissingValues(op.x[,i], impute.scale, impute.value)
  }
  for (i in seq_len(ncol(op.y))) {
    op.y[, i] = imputeMissingValues(op.y[,i], impute.scale, impute.value)
  }
  
  # get classes of Params (numeric or factor)
  classes.x = BBmisc::vcapply(op.x, function(x) class(x))
  classes.y = BBmisc::vcapply(op.y, function(x) class(x))
  
  # set and check x and y lims, if needed
  tmp = getOptPathLims(lim.x, lim.y, op, iter, 0.05)
  lim.x = tmp$lim.x
  lim.y = tmp$lim.y
  
  # set alpha and type values
  .alpha = if(alpha && iter > 0)
     normalize(dob, "range", range = c(1 / (iter + 1), 1)) else rep(1, length(dob))
  .type = as.factor(ifelse(dob == 0, "init", ifelse(dob == iter, "prop", "seq")))
  
  # Special case: X and Y are 1D
  if(dim.x == 1L && dim.y == 1L) {
    pl = plot2D(cbind(x = op.x, y = op.y), .alpha, .type, names = c(x.names, y.names), 
      space = "both", iter = iter, classes = c(classes.x, classes.y), 
      lim.x = lim.x[["XSpace"]], lim.y = lim.x[["YSpace"]], colours = colours, 
      size = size[1])
    return(list(plot = pl))
  }
  
  # plot 1: x-space
  if (dim.x == 1L && classes.x == "numeric") {
    pl1 = plot1DNum(op.x, .alpha, .type, names = x.names, space = "x", iter = iter, 
      lim.x = lim.x[["XSpace"]], colours = colours)
  }
  if (dim.x == 1L && classes.x == "factor") {
    pl1 = plot1DDisc(op.x, .alpha, .type, names = x.names, space = "x", iter = iter, 
      lim.y = lim.y[["XSpace"]], colours = colours)
  }
    
  if (dim.x == 2L) {
    pl1 = plot2D(op.x, .alpha, .type, names = x.names, space = "x", iter = iter,
      classes = classes.x, lim.x = lim.x[["XSpace"]], lim.y = lim.y[["XSpace"]], 
      colours = colours, size = size[1])
  } 
  if (dim.x > 2L) {
    pl1 = plotMultiD(op.x, .alpha, .type, names = x.names, space = "x", iter = iter, 
      colours = colours, size = size[2])
  }
  
  # plot 2: y-space
  if (dim.y == 1L && classes.y == "numeric") {
    pl2 = plot1DNum(op.y, .alpha, .type, names = y.names, space = "y", iter = iter,
      lim.x = lim.x[["YSpace"]], colours = colours)
  }
  if (dim.y == 1L && classes.y == "factor") {
    pl2 = plot1DDisc(op.y, .alpha, .type, names = y.names, space = "y", iter = iter, 
      lim.y = lim.y[["YSpace"]], colours = colours)
  }
  
  if (dim.y == 2L) {
    pl2 = plot2D(op.y, .alpha, .type, names = y.names, space = "y", iter = iter,
      classes = classes.y, lim.x = lim.x[["YSpace"]], lim.y = lim.y[["YSpace"]], 
      colours = colours, size = size[2])
  } 
  if (dim.y > 2L) {
    pl2 = plotMultiD(op.y, .alpha, .type, y.names, space = "y", iter = iter, 
      colours = colours, size = size[2])
  }
  
  return(list(plot.x = pl1, plot.y = pl2))
}


# Plot methods
#
# @param .alpha [\code{numeric}]\cr
#   Vector of alpha values for the points in the plots.
# @param .type [\code{factor}]\cr
#   Vector of types of the points, factor levels are init, seq and prob.
# @param names [\code{character}]\cr
#   Vector of the names of the variables.
# @param space[\code{character}]
#   If the X-Space is plotted, space = "x" and if the Y-Space is plotted, space = "y".
# @param iter [\code{integer(1)}]\cr
#   Current iteration.
# @param classes [\code{character(2)}]\cr
#  Classes of the variables (numeric or factor) in 2D plots.
# @param lim.x [\code{numeric(2)}], @param lim.y [\code{numeric(2)}]\cr
#  Limits for the x or y axis.
# @param colours [\code{character(3)}]\cr
#   Colours of the points/lines for the three point types init, seq and prob.
# @param size [\code{numeric(1)} | NULL]\cr
#   Size of points or lines. 
# @return A ggplot object.


# Plot method for a one-dimensional numeric X- or Y-Space
plot1DNum = function(op, .alpha, .type, names, space, iter, lim.x, colours) {
  
  op$.alpha = .alpha
  op$type = .type
  op$type = factor(op$type, levels = c("init", "seq", "prop"))
  
  if (space == "x") {  
    title = ggplot2::ggtitle("X-Space")    
  } 
  if (space == "y") {    
    title = ggplot2::ggtitle("Y-Space")    
  }
  
  pl = ggplot2::ggplot(op, ggplot2::aes_string(x = names[1]))
  pl = pl + ggplot2::geom_density(colour = "black")
  pl = pl + title
  pl = pl + ggplot2::geom_rug(ggplot2::aes_string(alpha = ".alpha", colour = "type"), 
    sides = "b", size = 2, data = op)
  pl = pl + ggplot2::xlim(lim.x)
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_alpha_continuous(range = c(1 / (iter + 1), 1))
  pl = pl + ggplot2::scale_colour_manual(values = c(init = colours[1], seq = colours[2], prop = colours[3]))
  pl = pl + ggplot2::theme(legend.position = "top")
return(pl)
}


# Plot method for a one-dimensional discrete X- or Y-Space
plot1DDisc = function(op, .alpha, .type, names, space, iter, lim.y, colours) {
  
  op$.alpha = as.factor(.alpha)
  op$type = .type
  op$type = factor(op$type, levels = c("init", "seq", "prop"))
  
  if (space == "x") {  
    title = ggplot2::ggtitle("X-Space")    
  } 
  if (space == "y") {    
    title = ggplot2::ggtitle("Y-Space")    
  }

  pl = ggplot2::ggplot(op, ggplot2::aes_string(x = names[1], fill = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_bar()
  pl = pl + title
  pl = pl + ggplot2::ylim(lim.y)
  pl = pl + ggplot2::scale_alpha_discrete(range = c(1 / (iter + 1), 1))
  pl = pl + ggplot2::scale_fill_manual(values = c(init = colours[1], seq = colours[2], prop = colours[3]))
  pl = pl + ggplot2::theme(legend.position = "top")
  pl = pl + ggplot2::guides(alpha = FALSE)

  return(pl)
}



# Plot method for a two-dimensional X- or Y-Space
plot2D = function(op, .alpha, .type, names, space, iter, classes, lim.x, lim.y, 
  colours, size) {
  
  op$.alpha = .alpha
  op$type = .type
  op$type = factor(op$type, levels = c("init", "seq", "prop"))
  
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
  
  if (any(classes == "factor")) {
    pos = ggplot2::position_jitter(w = 0.1, h = 0.1)
  } else {
    pos = "identity"
  }
  
  size = ifelse(is.null(size), 3, size)

  pl = ggplot2::ggplot(op, ggplot2::aes_string(
    x = names[1], y = names[2], shape = "type", colour = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_point(size = size, position = pos)
  pl = pl + title
  pl = pl + x.lab + y.lab
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_colour_manual(values = c(init = colours[1], seq = colours[2], prop = colours[3]))
  pl = pl + ggplot2::scale_shape_manual(values = c(init = 15, seq = 16, prop = 17))
  pl = pl + ggplot2::scale_alpha_continuous(range = c(1 / (iter + 1), 1))
  pl = pl + ggplot2::theme(legend.position = "top")
  if (classes[1] == "numeric") {
    pl = pl + ggplot2::xlim(lim.x)
  }
  if (classes[2] == "numeric") {
    pl = pl + ggplot2::ylim(lim.y)
  }
  return(pl)
}

# # Plot method for a two-dimensional X- or Y-Space when one variable is discrete 
# # and the other is numeric
# plot2DMixed = function(op, .alpha, .type, names, space, iter, classes, lim.y, colours) {
#   op$.alpha = .alpha
#   op$type = .type
#   op$type = factor(op$type, levels = c("init", "seq", "prop"))
#   
#   if (space == "x") {
#     title = ggplot2::ggtitle("X-Space")
#   } 
#   if (space == "y") {
#     title = ggplot2::ggtitle("Y-Space")
#   }
# 
#   # always plot the numeric variable on y-axis
#   if (classes[1] != classes[2]) {
#     name.x = names[classes == "factor"]
#     name.y = names[classes == "numeric"]
#   } else {
#     name.x = names[1]
#     name.y = names[2]
#   }
#     
#   pl = ggplot2::ggplot(op, ggplot2::aes_string(
#     x = name.x, y = name.y, fill = "type", colour = "type", alpha = ".alpha"))
#   pl = pl + ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", size = 3, 
#     stackgroups = TRUE, binpositions = "all", binwidth = diff(lim.y)/30)
#   pl = pl + title
#   pl = pl + ggplot2::guides(alpha = FALSE)
#   pl = pl + ggplot2::scale_fill_manual(values = c(init = colours[1], seq = colours[2], prop = colours[3]))
#   pl = pl + ggplot2::scale_colour_manual(values = c(init = colours[1], seq = colours[2], prop = colours[3]))
#   pl = pl + ggplot2::scale_alpha_continuous(range = c(1 / (iter + 1), 1))
#   pl = pl + ggplot2::theme(legend.position = "top")
#   # ylim only needed, if one param is numeric
#   if (any(classes == "numeric")) {
#     pl = pl + ggplot2::ylim(lim.y)
#   }
#   return(pl)
# }


# Plot method for a multi-dimensional X- or Y-Space
plotMultiD = function(op, .alpha, .type, names, space = "x", iter, colours, size) {
  args = list(columns = seq_along(names))
  for (i in seq_along(ncol(op))) {
    op[, i] = as.numeric(op[, i])
  }
  
  op$.alpha = .alpha
  op$type = .type
  op$type = factor(op$type, levels = c("init", "seq", "prop"))
  args$data = op
  args$alphaLines = ".alpha"
  args$groupColumn = ncol(op)
  # FIXME: aes searches for size in the global environment
  size <<- ifelse(is.null(size), 1.5, size)
  args$mapping = ggplot2::aes(lwd = size)
  
  if (space == "x") {
    title = ggplot2::ggtitle("X-Space")
  } else {
    title = ggplot2::ggtitle("Y-Space")
  }
  

  pl = do.call(GGally::ggparcoord, args)
  pl = pl + ggplot2::ylab ("value divided by standard deviation")
  pl = pl + title
  pl = pl + ggplot2::guides(alpha = FALSE, size = FALSE)
  pl = pl + ggplot2::scale_colour_manual(values = c(init = colours[1], seq = colours[2], prop = colours[3]))
  pl = pl + ggplot2::theme(legend.position = "top", legend.margin = grid::unit(0.05, "cm"))
  return(pl)
}