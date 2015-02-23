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

# If lim is NULL, lim is checked to be a list of same length as number of cols
# in op, and each element numeric vector of length 2.
# Otherwise lim is determined as min and max for each dimension of op, and
# increased by scale * range of dimension.
getOptPathLims <- function(lim, op, scale) {
  assertNumeric(scale, len = 1L, lower = 0, finite = TRUE, any.missing = FALSE)
  dim = ncol(op)
  if(is.null(lim)) {
    .min = apply(op, 2, min)
    .max = apply(op, 2, max)
    .min = .min - scale * (.max - .min)
    .max = .max + scale * (.max - .min)
    lim = lapply(1:dim, function(i) c(.min[i], .max[i]))
  } else {
    assertList(lim, len = dim, any.missing = FALSE)
    for (i in 1:dim)
      assertNumeric(lim[[i]], len = 2L)
  }
  lim
}


