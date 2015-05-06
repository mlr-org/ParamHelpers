stopIfLearnerParams = function(par.set) {
  if(any(sapply(par.set$pars, function(x) inherits(x, "LearnerParameter"))))
    stop("No par.set parameter in 'generateDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")
}


stopIfFunOrUntypedParams = function(par.set) {
  types = getParamTypes(par.set)
  not.ok = intersect(c("untyped", "function"), types)
  if (length(not.ok) > 0L)
    stopf("Parameters of this type are not allowed, but were found: %s", collapse(not.ok))
}

doBasicGenDesignChecks = function(par.set) {
  assertClass(par.set, "ParamSet")
  if (isEmpty(par.set))
    stop("par.set must not be empty!")
  stopIfLearnerParams(par.set)
  stopIfFunOrUntypedParams(par.set)

  lower = getLower(par.set, with.nr = TRUE)
  upper = getUpper(par.set, with.nr = TRUE)

  if (any(is.infinite(c(lower, upper))))
    stop("Finite box constraints required!")

  return(list(lower = lower, upper = upper))
}


getParamNA = function(par, repeated = FALSE) {
  v = switch(par$type,
    numeric = NA_real_,
    numericvector = NA_real_,
    integer = NA_integer_,
    integervector = NA_integer_,
    discrete = NA_character_,
    discretevector = NA_character_,
    logical = NA,
    logicalvector = NA
  )
  if (repeated)
    v = rep(v, par$len)
  return(v)
}

# Check the given X and Y limit list. Each list can have 2 elements:
# XSpace and YSpace. If the element is NULL, it is set, otherwise it is
# checked. If the dimensionality of X and Y space is greater than 2, the limits
# are set to NULL. The same happens if there is a discrete variable in the 2D case.
# We don't need limits in this cases. 
# If the dimensionality is 1 for X or Y Space, for this space the lim.y is NULL.
getOptPathLims <- function(lim.x, lim.y, op.x, op.y, iters, scale) {
  assertNumeric(scale, len = 1L, lower = 0, finite = TRUE, any.missing = FALSE)
  
  assertList(lim.x)
  assertList(lim.y)
  for (space in c("XSpace", "YSpace")) {
    op.frame = if (space == "XSpace") op.x else op.y
    dim = ncol(op.frame)
    classes = BBmisc::vcapply(op.frame, function(x) class(x))
    if (dim > 2L) {
      lim.x[[space]] = NULL
      lim.y[[space]] = NULL
      next
    }
    
    if (all(classes == "numeric")) {
      if (is.null(lim.x[[space]])) {
        lim.x[[space]] = range(op.frame[, 1])
        lim.x[[space]] = c(-1, 1) * scale * abs(diff(lim.x[[space]])) + lim.x[[space]]
      } else {
        assertNumeric(lim.x[[space]], len = 2L, any.missing = FALSE)
      }
    }
    
    # limits for barplot (1D discrete case)
    if (dim == 1L && classes == "factor") {
      lim.x[[space]] = NULL
      if (is.null(lim.y[[space]])) {
        lim.y[[space]] = c(0, max(table(op.frame)))
      } else {
        assertNumeric(lim.y[[space]], len = 2L, any.missing = FALSE)
      }
    }
    
    if (dim == 2L) {
      if (classes[1] == "numeric") {
        if (is.null(lim.x[[space]])) {
          lim.x[[space]] = range(op.frame[, 1])
          lim.x[[space]] = c(-1, 1) * scale * abs(diff(lim.x[[space]])) + lim.x[[space]]
        } else {
          assertNumeric(lim.x[[space]], len = 2L, any.missing = FALSE)
        }
      } else {
        lim.x[[space]] = NULL
      }
      
      if (classes[2] == "numeric") {
        if (is.null(lim.y[[space]])) {
          lim.y[[space]] = range(op.frame[, 2])
          lim.y[[space]] = c(-1, 1) * scale * abs(diff(lim.y[[space]])) + lim.y[[space]]
        } else {
          assertNumeric(lim.y[[space]], len = 2L, any.missing = FALSE)
        }
      } else {
        lim.y[[space]] = NULL
      } 
    }
    
  }
  return(list(lim.x = lim.x, lim.y = lim.y))
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
getAndSubsetPlotData = function(op, iters, subset.obs, subset.vars, subset.targets,
  marked = NULL, alpha = TRUE, ...) {
  
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
  if (is.numeric(subset.vars))
    assertIntegerish(subset.vars, lower = 1, upper = dim.x, unique = TRUE, any.missing = FALSE)
  else 
    assertSubset(subset.vars, x.names)
  
  if (missing(subset.targets))
    subset.targets = y.names
  if (is.numeric(subset.targets))
    assertIntegerish(subset.targets, lower = 1, upper = getOptPathLength(op), unique = TRUE, 
      any.missing = FALSE)
  else
    assertSubset(subset.targets, y.names)
  
  op.x = op.x[subset.obs, subset.vars, drop = FALSE]
  op.y = op.y[subset.obs, subset.targets, drop = FALSE]
  dob = dob[subset.obs]
  .alpha = .alpha[subset.obs]
  .type = .type[subset.obs]
  
  return(
    list(
      op.x = op.x,
      op.y = op.y,
      dob = dob,
      .alpha = .alpha,
      .type = .type,
      subset.obs = subset.obs,
      subset.vars = subset.vars,
      subset.targets = subset.targets
    )
  )
}
