# Check the given X and Y limit list. Each list can have 2 elements:
# XSpace and YSpace. If the element is NULL, it is set, otherwise it is
# checked. If the dimensionality of X and Y space is greater than 2, the limits
# are set to NULL. The same happens if there is a discrete variable in the 2D case.
# We don't need limits in this cases. 
# If the dimensionality is 1 for X or Y Space, for this space the ylim is NULL.
getOptPathLims = function(xlim, ylim, op.x, op.y, iters, scale) {
  
  assertNumeric(scale, len = 1L, lower = 0L, finite = TRUE, any.missing = FALSE)
  
  assertList(xlim)
  assertList(ylim)
  for (space in c("XSpace", "YSpace")) {
    op.frame = if (space == "XSpace") op.x else op.y
    dim = ncol(op.frame)
    classes = BBmisc::vcapply(op.frame, function(x) class(x))
    if (dim > 2L) {
      xlim[[space]] = NULL
      ylim[[space]] = NULL
      next
    }
    
    if (all(classes == "numeric")) {
      if (is.null(xlim[[space]])) {
        xlim[[space]] = range(op.frame[, 1L])
        xlim[[space]] = c(-1, 1) * scale * abs(diff(xlim[[space]])) + xlim[[space]]
      } else {
        assertNumeric(xlim[[space]], len = 2L, any.missing = FALSE)
      }
    }
    
    # limits for barplot (1D discrete case)
    if (dim == 1L && classes == "factor") {
      xlim[[space]] = NULL
      if (is.null(ylim[[space]])) {
        ylim[[space]] = c(0, max(table(op.frame)))
      } else {
        assertNumeric(ylim[[space]], len = 2L, any.missing = FALSE)
      }
    }
    
    if (dim == 2L) {
      if (classes[1L] == "numeric") {
        if (is.null(xlim[[space]])) {
          xlim[[space]] = range(op.frame[, 1L])
          xlim[[space]] = c(-1, 1) * scale * abs(diff(xlim[[space]])) + xlim[[space]]
        } else {
          assertNumeric(xlim[[space]], len = 2L, any.missing = FALSE)
        }
      } else {
        xlim[[space]] = NULL
      }
      
      if (classes[2L] == "numeric") {
        if (is.null(ylim[[space]])) {
          ylim[[space]] = range(op.frame[, 2L])
          ylim[[space]] = c(-1, 1) * scale * abs(diff(ylim[[space]])) + ylim[[space]]
        } else {
          assertNumeric(ylim[[space]], len = 2L, any.missing = FALSE)
        }
      } else {
        ylim[[space]] = NULL
      } 
    }
    
  }
  return(list(xlim = xlim, ylim = ylim))
}

# Function to impute missing values. 
imputeMissingValues = function(x, impute.scale, impute.value) {
  na.index = which(is.na(x))
  if (length(na.index) > 0) {
    if (class(x) == "numeric") {
      x[na.index] = max(x, na.rm = TRUE) + impute.scale * (diff(range(x, na.rm = TRUE)))
    } 
    if (class(x) == "factor") {
      levels(x) = c(levels(x), impute.value)
      x[na.index] = impute.value
    }
  }
  return(x)
}

# subset rows and cols of the opt.path and return list with data.frames for
# x and y space and the subsets.
# returns list with dataframes for x and y space, vectors for dob, type and alpha
# character vector for x and y names
getAndSubsetPlotData = function(op, iters, subset.obs, subset.vars, subset.targets,
  marked = NULL, alpha = TRUE, impute.scale = 0.05, impute.value = "missing", ...) {
  
  x.names = colnames(getOptPathX(op))
  y.names = op$y.names
  dim.x = length(x.names)
  dim.y = length(y.names)
  iters.max = max(getOptPathDOB(op))
  
  op.x = as.data.frame(op, include.x = TRUE, include.y = FALSE,
    include.rest = FALSE, dob = 0:max(iters), eol = c(min(iters):iters.max, NA))
  op.y = as.data.frame(op, include.x = FALSE, include.y = TRUE,
    include.rest = FALSE, dob = 0:max(iters), eol = c(min(iters):iters.max, NA))
  dob = getOptPathDOB(op, dob = 0:max(iters), eol = c((max(iters) + 1):iters.max, NA))
  
  # mark best point / pareto front if marked = "best"
  if (is.character(marked)) {
    if(length(y.names) == 1) {
      marked = getOptPathBestIndex(op)
    } else {
      marked = getOptPathParetoFront(op, index = TRUE)
    }
  }
  
  # make sure that only points are marked that are alive at this iteration
  marked = marked[marked <= nrow(op.x)]
  
  # set alpha and type values
  .alpha = if(alpha && max(iters) > 0)
    normalize(dob, "range", range = c(1 / (max(iters) + 1), 1)) else rep(1, length(dob))
  .type = as.factor(ifelse(dob == 0, "init", ifelse(dob == max(iters), "prop", "seq")))
  .type = factor(.type, levels = c("init", "seq", "prop", "marked"))
  if (!is.null(marked)) {
    .type[marked] = "marked"
  }
  .alpha = pmax(0.1, .alpha)
  
  # And now subset everything
  if (missing(subset.obs))
    subset.obs = 1:nrow(op.x)
  assertIntegerish(subset.obs, lower = 1, upper = getOptPathLength(op), unique = TRUE, 
    any.missing = FALSE)
  # use only indices avaible in the current iterations
  subset.obs = subset.obs[subset.obs <= nrow(op.x)]
  
  if (missing(subset.vars))
    subset.vars = x.names
  if (is.numeric(subset.vars)) {
    assertIntegerish(subset.vars, lower = 1, upper = dim.x, unique = TRUE, any.missing = FALSE)
  }
  else 
    assertSubset(subset.vars, x.names)
  
  if (missing(subset.targets))
    subset.targets = y.names
  if (is.numeric(subset.targets)) {
    assertIntegerish(subset.targets, lower = 1, upper = getOptPathLength(op), unique = TRUE, 
      any.missing = FALSE)
  }
  else
    assertSubset(subset.targets, y.names)
  
  # impute missing values
  op.x = BBmisc::dapply(op.x, fun = imputeMissingValues, impute.scale = impute.scale,
    impute.value = impute.value)
  op.y = BBmisc::dapply(op.y, fun = imputeMissingValues, impute.scale = impute.scale,
    impute.value = impute.value)
  
  op.x = op.x[subset.obs, subset.vars, drop = FALSE]
  op.y = op.y[subset.obs, subset.targets, drop = FALSE]
  dob = dob[subset.obs]
  .alpha = .alpha[subset.obs]
  .type = .type[subset.obs]
  x.names = if (is.numeric(subset.vars)) x.names[subset.vars] else subset.vars
  y.names = if (is.numeric(subset.targets)) x.names[subset.targets] else subset.targets
  
  return(
    list(
      op.x = op.x,
      op.y = op.y,
      dob = dob,
      .alpha = .alpha,
      .type = .type,
      x.names = x.names,
      y.names = y.names
    )
  )
}
