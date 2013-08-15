#' @rdname Param
#' @export 
makeNumericParam = function(id, lower=-Inf, upper=Inf, trafo=NULL, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(lower, "numeric", len=1, na.ok=FALSE)
  checkArg(upper, "numeric", len=1, na.ok=FALSE)
  if (!is.null(trafo))
    checkArg(trafo, "function")
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  if (upper < lower)
    stop("No possible value!")
  makeParam(id, "numeric", 1L, lower, upper, NULL, trafo, requires)
} 

#' @rdname Param
#' @export 
makeNumericVectorParam = function(id, len, lower=-Inf, upper=Inf, trafo=NULL, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=FALSE)
  if (is.numeric(lower) && length(lower) == 1)
    lower = rep(lower, len)
  if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, len)
  checkArg(lower, "numeric", min.len=1, na.ok=FALSE)
  checkArg(upper, "numeric", min.len=1, na.ok=FALSE)
  if (!is.null(trafo))
    checkArg(trafo, "function")
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  if (any(upper < lower))
    stop("No possible value!")
  makeParam(id, "numericvector", len, lower, upper, NULL, trafo, requires)
} 

#' @rdname Param
#' @export 
makeIntegerParam = function(id, lower=-Inf, upper=Inf, trafo=NULL, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(lower, "numeric", len=1, na.ok=FALSE)
  checkArg(upper, "numeric", len=1, na.ok=FALSE)
  if (!is.null(trafo))
    checkArg(trafo, "function")
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  if (upper < lower)
    stop("No possible value!")
  makeParam(id, "integer", 1L,  lower, upper, NULL, trafo, requires)
} 

#' @rdname Param
#' @export 
makeIntegerVectorParam = function(id, len, lower=-Inf, upper=Inf, trafo=NULL, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=FALSE)
  if (is.numeric(lower) && length(lower) == 1)
    lower = rep(lower, len)
  if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, len)
  checkArg(lower, "numeric", min.len=1, na.ok=FALSE)
  checkArg(upper, "numeric", min.len=1, na.ok=FALSE)
  if (!is.null(trafo))
    checkArg(trafo, "function")
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  if (any(upper < lower))
    stop("No possible value!")
  makeParam(id, "integervector", len,  lower, upper, NULL, trafo, requires)
} 

#' @rdname Param
#' @export 
makeLogicalParam = function(id, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  makeParam(id, "logical", 1L, NULL, NULL, values, requires=requires)
} 

#' @rdname Param
#' @export 
makeLogicalVectorParam = function(id, len, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  values = list(TRUE, FALSE)
  names(values) = c("TRUE", "FALSE")
  makeParam(id, "logicalvector", len, NULL, NULL, values, requires=requires)
} 

#' @rdname Param
#' @export 
makeDiscreteParam = function(id, values, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  values = checkValuesForDiscreteParam(id, values)
  makeParam(id, "discrete", 1L, NULL, NULL, values, requires=requires)
} 

#' @rdname Param
#' @export 
makeDiscreteVectorParam = function(id, len, values, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  values = checkValuesForDiscreteParam(id, values)
  makeParam(id, "discretevector", len, NULL, NULL, values, requires=requires)
} 

#' @rdname Param
#' @export 
makeFunctionParam = function(id, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  makeParam(id, "function", 1L, NULL, NULL, NULL, requires=requires)
} 

#FIXME what happens if NA is later used for untyped params? because we might interpret this as
# missing value wrt. dependent params
#' @rdname Param
#' @export 
makeUntypedParam = function(id, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  makeParam(id, "untyped", 1L, NULL, NULL, NULL, requires=requires)
} 


##### small helpers #####

checkValuesForDiscreteParam = function(id, values) {
  if (is.vector(values))
    values = as.list(values)
  checkArg(values, "list")

  if (length(values) == 0L)
    stopf("No possible value for discrete parameter %s!", id)

  # check that NA does not occur in values, we use that for "missing state" for dependent params
  # make sure that this works for complex object too, cannot be done with simple is.na
  if (any(sapply(values, isScalarNA)))
    stopf("NA is not allowed as a value for discrete parameter %s.\nParamHelpers uses NA as a special value for dependent parameters.", id)
  
  n = length(values)
  ns = names(values)
  # if names missing, set all to ""
  if (is.null(ns))
    ns = rep("", n)
  # guess missing names
  for (i in seq_len(n)) {
    v = values[[i]]
    if(is.na(ns[i]) || ns[i] == "") {
      if (is.character(v) || is.numeric(v))
        ns[i] = as.character(v)
    }
  }  
  names(values) = ns
  if(!isProperlyNamed(values)) {
    stop("Not all values for parameter %s were named and names could not be guessed!", id)
  }
  
  # check that NA does not occur in value names, see above
  if ("NA" %in% names(values))  
    stopf("NA is not allowed as a value name for discrete parameter %s.\nParamHelpers uses NA as a special value for dependent parameters.", id)
  
  return(values)
}