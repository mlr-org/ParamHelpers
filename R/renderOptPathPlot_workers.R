# Plot methods
# @param op 
#   The optimization path
# @param .alpha [\code{numeric}]\cr
#   Vector of alpha values for the points in the plots.
# @param .type [\code{factor}]\cr
#   Vector of types of the points, factor levels are init, seq, prob and marked.
# @param names [\code{character}]\cr
#   Vector of the names of the variables. Used to identify variables for the plot.
# @param short.names [\code{character}]\cr
#   Vector of the short names of the variables. This names will be printed in the plot!
# @param space[\code{character}]
#   If the X-Space is plotted, space = "x", if the Y-Space is plotted, space = "y".
#   Special case 1D -> 1D also "both" is possible.
# @param iter [\code{integer(1)}]\cr
#   Current iteration.
# @param classes [\code{character(2)}]\cr
#  Classes of the variables (numeric or factor) in 2D plots.
# @param xlim, ylim [\code{numeric(2)}]\cr
#  Limits for the x and y axis respectively.
# @param colours [\code{character(4)}]\cr
#   Colours of the points/lines for the three point types init, seq, prob and marked.
# @param size [\code{numeric(1)} | NULL]\cr
#   Size of points / lines. 
# @return A ggplot object.


# Plot method for a one-dimensional numeric X- or Y-Space
plot1DNum = function(op, .alpha, .type, names, short.names, space, iter, xlim, colours, ggplot.theme) {
  
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
  pl = pl + ggplot2::xlab(short.names)
  pl = pl + ggplot2::geom_rug(ggplot2::aes_string(alpha = ".alpha", colour = "type"), 
    sides = "b", size = 2L, data = op)
  pl = pl + ggplot2::coord_cartesian(xlim = xlim) 
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_alpha_continuous(range = c(max(1 / (iter + 1), 0.1), 1L))
  pl = pl + ggplot2::scale_colour_manual(
    values = c(init = colours[1L], seq = colours[2L], prop = colours[3L], marked = colours[4L]))
  pl = pl + ggplot.theme
  
  return(pl)
}


# Plot method for a one-dimensional discrete X- or Y-Space
plot1DDisc = function(op, .alpha, .type, names, short.names, space, iter, ylim, 
  colours, ggplot.theme) {
  
  op$.alpha = as.factor(.alpha)
  op$type = .type
  
  if (space == "x") {  
    title = ggplot2::ggtitle("X-Space")    
  } 
  if (space == "y") {    
    title = ggplot2::ggtitle("Y-Space")    
  }
  
  pl = ggplot2::ggplot(op, ggplot2::aes_string(x = names[1L], fill = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_bar()
  pl = pl + title
  pl = pl + ggplot2::xlab(short.names)
  pl = pl + ggplot2::ylim(ylim)
  pl = pl + ggplot2::scale_alpha_discrete(range = c(max(1 / (iter + 1), 0.1), 1L))
  pl = pl + ggplot2::scale_fill_manual(
    values = c(init = colours[1L], seq = colours[2L], prop = colours[3L], marked = colours[4L]))
  pl = pl + ggplot.theme
  pl = pl + ggplot2::guides(alpha = FALSE)
  
  return(pl)
}


# Plot method for a two-dimensional X- or Y-Space

plot2D = function(op, .alpha, .type, names, short.names, space, iter, classes, xlim, ylim, 
  colours, size, ggplot.theme) {
  
  op$.alpha = .alpha
  op$type = .type
  
  if (space == "x") {
    title = ggplot2::ggtitle("X-Space")
  } 
  if (space == "y") {
    title = ggplot2::ggtitle("Y-Space")
  }
  if (space == "both") {
    title = ggplot2::ggtitle("X- and Y-Space")
  }
  
  if (any(classes == "factor")) {
    pos = ggplot2::position_jitter(w = 0.1, h = 0.1)
  } else {
    pos = "identity"
  }
  
  pl = ggplot2::ggplot(op, ggplot2::aes_string(
    x = names[1L], y = names[2L], shape = "type", colour = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_point(size = size, position = pos)
  pl = pl + title
  pl = pl + ggplot2::xlab(short.names[1L]) + ggplot2::ylab(short.names[2L])
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_colour_manual(name = "type",
    values = c(init = colours[1L], seq = colours[2L], prop = colours[3L], marked = colours[4L]))
  pl = pl + ggplot2::scale_shape_manual(name = "type", 
    values = c(init = 15L, seq = 16L, prop = 17L, marked = 18L))
  pl = pl + ggplot2::scale_alpha_continuous(range = c(max(1 / (iter + 1), 0.1), 1L))
  pl = pl + ggplot.theme
  if (classes[1L] == "numeric") {
    pl = pl + ggplot2::xlim(xlim)
  }
  if (classes[2L] == "numeric") {
    pl = pl + ggplot2::ylim(ylim)
  }
  return(pl)
}

# Plot method for a multi-dimensional X- or Y-Space
plotMultiD = function(op, .alpha, .type, names, short.names, space, iter, colours, size, 
  scale, ggplot.theme) {
  args = list(columns = seq_along(names))
  for (i in seq_along(ncol(op))) {
    op[, i] = as.numeric(op[, i])
  }
  
  op$.alpha = .alpha
  # minimal alpha value:
  op$type = .type
  args$data = op
  args$alphaLines = ".alpha"
  args$groupColumn = ncol(op)
  args$scale = scale
  args$mapping = ggplot2::aes_q(lwd = size)
  
  
  if (space == "x") {
    title = ggplot2::ggtitle("X-Space")
  } else {
    title = ggplot2::ggtitle("Y-Space")
  }
  pl = do.call(GGally::ggparcoord, args)
  pl = pl + ggplot2::ylab ("value divided by standard deviation")
  pl = pl + ggplot2::scale_x_discrete(labels = short.names)
  pl = pl + title
  pl = pl + ggplot2::guides(alpha = FALSE, size = FALSE)
  pl = pl + ggplot2::scale_colour_manual(
    values = c(init = colours[1L], seq = colours[2L], prop = colours[3L], marked = colours[4L]))
  pl = pl + ggplot.theme
  return(pl)
}


# Function to plot one or more numeric variables over time
# names: all corresponding variables must be numeric
# short.names: short names of the variables given by names
plotVariablesOverTime = function(op, .alpha, .type, names, short.names, space, 
  iter, lim.y, colours, ggplot.theme) {
  
  op2 = op[, c(names, "dob")]
  op2$.alpha = .alpha
  op2$type = .type
  
  # remove all point with dob = 0:
  op2 = op2[op2$dob != 0, ]
  
  op2 = reshape(op2, ids = row.names(op2),
                times = names, timevar = "variable",
                varying = list(names), direction = "long", v.names = c("value"))
  
  
  if (space == "x") {  
    title = ggplot2::ggtitle("X-Space")    
  } 
  if (space == "y") {    
    title = ggplot2::ggtitle("Y-Space")    
  }
  
  pl = ggplot2::ggplot(op2, ggplot2::aes_string(x = "dob", y = "value", group = "variable", 
                                                linetype = "variable"))
  pl = pl + ggplot2::geom_point()
  pl = pl + ggplot2::geom_line() 
  pl = pl + ggplot2::scale_linetype_discrete(labels = short.names)
  pl = pl + ggplot2::ylim(lim.y)
  pl = pl + title
  pl = pl + ggplot.theme
  
  return(pl)
}
