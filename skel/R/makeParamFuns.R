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
  if (is.vector(values))
    values = as.list(values)
  checkArg(values, "list")
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  if (length(values)==0)
    stop("No possible value!")
  n = length(values)
  # if names missing, set all to ""
  if (is.null(names(values)))
    names(values) = rep("", n)
  # guess missing names
  ns = names(values)
  for (i in 1:n) {
    v = values[[i]]
    if(is.na(ns[i]) || ns[i] == "") {
      if (is.character(v) || is.numeric(v))
        names(values)[i] = as.character(v)
    }
  }  
  if(!isProperlyNamed(values)) {
    stop("Not all values for par. ", id,  " were named and names could not be guessed!")
  }
  if(any(duplicated(names(values))))
    stop("Not all names for par. ", id,  " are unique!")
  makeParam(id, "discrete", 1L, NULL, NULL, values, requires=requires)
} 

#' @rdname Param
#' @export 
makeDiscreteVectorParam = function(id, len, values, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=FALSE)
  if (is.vector(values))
    values = as.list(values)
  checkArg(values, "list")
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  if (length(values)==0)
    stop("No possible value!")
  n = length(values)
  # if names missing, set all to ""
  if (is.null(names(values)))
    names(values) = rep("", n)
  # guess missing names
  ns = names(values)
  for (i in 1:n) {
    v = values[[i]]
    if(is.na(ns[i]) || ns[i] == "") {
      if (is.character(v) || is.numeric(v))
        names(values)[i] = as.character(v)
    }
  }  
  if(!isProperlyNamed(values)) {
    stop("Not all values for par. ", id,  " were named and names could not be guessed!")
  }
  if(any(duplicated(names(values))))
    stop("Not all names for par. ", id,  " are unique!")
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

#' @rdname Param
#' @export 
makeUntypedParam = function(id, requires=NULL) {
  checkArg(id, "character", len=1, na.ok=FALSE)
  if (!is.null(requires))
    checkArg(requires, c("call", "expression"))
  makeParam(id, "untyped", 1L, NULL, NULL, NULL, requires=requires)
} 




