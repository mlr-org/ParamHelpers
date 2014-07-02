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

