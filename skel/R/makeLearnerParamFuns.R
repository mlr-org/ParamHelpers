#' @rdname LearnerParam
#' @export 
makeNumericLearnerParam = function(id, lower=-Inf, upper=Inf, default, 
  when="train", requires=NULL) {
  
  p = makeNumericParam(id, lower, upper)
  learnerParamFromParam(p, default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeNumericVectorLearnerParam = function(id, len=as.integer(NA), lower=-Inf, 
  upper=Inf, default, when="train", requires=NULL) {
  
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=TRUE)
  if (is.na(len))
    p = makeNumericVectorParam(id, len=1, lower=lower, upper=upper)
  else  
    p = makeNumericVectorParam(id, len=len, lower=lower, upper=upper)
  p = learnerParamFromParam(p, default, when, requires)
  p$len = len
  return(p)
}


#' @rdname LearnerParam
#' @export 
makeIntegerLearnerParam = function(id, lower=-Inf, upper=Inf,
  default, when="train", requires=NULL) {
  
  p = makeIntegerParam(id, lower, upper)
  learnerParamFromParam(p, default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeIntegerVectorLearnerParam = function(id, len=as.integer(NA), lower=-Inf, 
  upper=Inf, default, when="train", requires=NULL) {
  
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=TRUE)
  if (is.na(len))
    p = makeIntegerVectorParam(id, len=1, lower=lower, upper=upper)
  else  
    p = makeIntegerVectorParam(id, len=len, lower=lower, upper=upper)
  p = learnerParamFromParam(p, default, when, requires)
  p$len = len
  return(p)
}

#' @rdname LearnerParam
#' @export 
makeDiscreteLearnerParam = function(id, values, default,  
  when="train", requires=NULL) {
  
  p = makeDiscreteParam(id, values)
  learnerParamFromParam(p, default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeDiscreteVectorLearnerParam = function(id, len=as.integer(NA), values, default,  
  when="train", requires=NULL) {
  
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=TRUE)
  if (is.na(len))
    p = makeDiscreteVectorParam(id, len=1, values=values)
  else  
    p = makeDiscreteVectorParam(id, len=len, values=values)
  p = learnerParamFromParam(p, default, when, requires)
  p$len = len
  return(p)
}

#' @rdname LearnerParam
#' @export 
makeLogicalLearnerParam = function(id, default, when="train",
  requires=NULL) {
  
  p = makeLogicalParam(id)
  learnerParamFromParam(p, default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeLogicalVectorLearnerParam = function(id, len=as.integer(NA), default, when="train",
  requires=NULL) {
  
  len = convertInteger(len)
  checkArg(len, "integer", len=1, na.ok=TRUE)
  if (is.na(len))
    p = makeLogicalVectorParam(id, len=1)
  else  
    p = makeLogicalVectorParam(id, len=len)
  p = learnerParamFromParam(p, default, when, requires)
  p$len = len
  return(p)
}

#' @rdname LearnerParam
#' @export 
makeUntypedLearnerParam = function(id, default, when="train", requires=NULL) {
  p = makeUntypedParam(id)
  learnerParamFromParam(p, default, when, requires)
}

#' @rdname LearnerParam
#' @export 
makeFunctionLearnerParam = function(id, default, when="train", requires=NULL) {
  p = makeFunctionParam(id)
  learnerParamFromParam(p, default, when, requires)
}

learnerParamFromParam = function(p, default, when, requires) {
  checkArg(when, choices=c("train", "predict", "both"))
  if (!is.null(requires))
    checkArg(requires, "expression")
  if (!missing(default) && !isFeasible(p, default))
    stop(p$id, " : 'default' must be missing or a feasible parameter setting.")  
  has.default = !missing(default)
  if (missing(default))
    default = NULL
  makeLearnerParam(p, has.default, default, when, requires)
}
