# Plot methods
#
# @param .alpha [\code{numeric}]\cr
#   Vector of alpha values for the points in the plots.
# @param .type [\code{factor}]\cr
#   Vector of types of the points, factor levels are init, seq, prob and marked.
# @param names [\code{character}]\cr
#   Vector of the names of the variables.
# @param short.names [\code{character}]\cr
#   Vector of the short names of the variables.
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
plot1DNum = function(op, .alpha, .type, names, short.names, space, iter, lim.x, colours, ggplot.theme) {
  
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
    sides = "b", size = 2, data = op)
  pl = pl + ggplot2::coord_cartesian(xlim = lim.x) 
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_alpha_continuous(range = c(max(1 / (iter + 1), 0.1), 1))
  pl = pl + ggplot2::scale_colour_manual(
    values = c(init = colours[1], seq = colours[2], prop = colours[3], marked = colours[4]))
  pl = pl + ggplot.theme
  
  return(pl)
}


# Plot method for a one-dimensional discrete X- or Y-Space
plot1DDisc = function(op, .alpha, .type, names, short.names, space, iter, lim.y, 
  colours, ggplot.theme) {
  
  op$.alpha = as.factor(.alpha)
  op$type = .type
  
  if (space == "x") {  
    title = ggplot2::ggtitle("X-Space")    
  } 
  if (space == "y") {    
    title = ggplot2::ggtitle("Y-Space")    
  }
  
  pl = ggplot2::ggplot(op, ggplot2::aes_string(x = names[1], fill = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_bar()
  pl = pl + title
  pl = pl + ggplot2::xlab(short.names)
  pl = pl + ggplot2::ylim(lim.y)
  pl = pl + ggplot2::scale_alpha_discrete(range = c(max(1 / (iter + 1), 0.1), 1))
  pl = pl + ggplot2::scale_fill_manual(
    values = c(init = colours[1], seq = colours[2], prop = colours[3], marked = colours[4]))
  pl = pl + ggplot.theme
  pl = pl + ggplot2::guides(alpha = FALSE)
  
  return(pl)
}



# Plot method for a two-dimensional X- or Y-Space

plot2D = function(op, .alpha, .type, names, short.names, space, iter, classes, lim.x, lim.y, 
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
    x = names[1], y = names[2], shape = "type", colour = "type", alpha = ".alpha"))
  pl = pl + ggplot2::geom_point(size = size, position = pos)
  pl = pl + title
  pl = pl + ggplot2::xlab(short.names[1]) + ggplot2::ylab(short.names[2])
  pl = pl + ggplot2::guides(alpha = FALSE)
  pl = pl + ggplot2::scale_colour_manual(name = "type",
    values = c(init = colours[1], seq = colours[2], prop = colours[3], marked = colours[4]))
  pl = pl + ggplot2::scale_shape_manual(name = "type", 
    values = c(init = 15, seq = 16, prop = 17, marked = 18))
  pl = pl + ggplot2::scale_alpha_continuous(range = c(max(1 / (iter + 1), 0.1), 1))
  pl = pl + ggplot.theme
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
    values = c(init = colours[1], seq = colours[2], prop = colours[3], marked = colours[4]))
  pl = pl + ggplot.theme
  return(pl)
}
