#' Create a description object for a parameter.
#' 
#' For each parameter type a special constructor function is available, see below. 
#' 
#' The S3 class is a list which stores these elements:
#' \describe{
#' \item{id [\code{character(1)}]}{See argument of same name.}
#' \item{type [\code{character(1)}]}{Data type of parameter. Possible types are \dQuote{numeric}, \dQuote{numericvector}, \dQuote{integer}, \dQuote{integervector}, \dQuote{logical}, \dQuote{logicalvector}, \dQuote{discrete}, \dQuote{discretevector}, \dQuote{function}, \dQuote{untyped}.}
#' \item{len [\code{integer(1)}]}{See argument of same name.}
#' \item{lower [\code{numeric}]}{See argument of same name. Length of this vector is \code{len}.}
#' \item{upper [\code{numeric}]}{See argument of same name. Length of this vector is \code{len}.}
#' \item{values [\code{list}]}{Discrete values, always stored as a named list.}
#' \item{trafo [\code{NULL} | \code{function(x)}]}{See argument of same name.}
#' \item{requires [\code{NULL} | \code{expression}]}{See argument of same name.}
#' }
#' 
#' @param id [\code{character(1)}]\cr
#'   Name of parameter.
#' @param len [\code{integer(1)}]\cr
#'   Length of vector parameter.
#' @param lower [\code{numeric}]\cr
#'   Lower bounds. 
#'   A singe value of length 1 is automatically replicated to \code{len} for vector parameters. 
#'   Default is \code{-Inf}.
#' @param upper [\code{numeric}]\cr
#'   Upper bounds. 
#'   A singe value of length 1 is automatically replicated to \code{len} for vector parameters. 
#'   Default is \code{Inf}.
#' @param values [\code{vector} | \code{list}]\cr
#'   Possible discrete values. Instead of using a vector of atomic values,
#'   you are also allowed to pass a list of quite \dQuote{complex} R objects,
#'   which are used as discrete choices. If you do the latter,
#'   the elements must be uniquely named, so that the names can be used
#'   as internal represenatations for the choice.    
#' @param trafo [\code{NULL} | \code{function(x)}]\cr
#'   Function to transform parameter. It should be applied to the parameter value 
#'   before it is, e.g., passed to a corresponding objective function. 
#'   Function must accept a parameter value as the first argument and return a transformed one.
#'   Default is \code{NULL} which means no transformation.   
#' @param requires [\code{NULL} | R expression]\cr
#'   States requirements on other paramaters' values, so that setting
#'   this parameter only makes sense if its requirements are satisfied (dependent parameter).
#'   Only really useful if the parameter is included in a \code{\link{ParamSet}}.
#'   Default is \code{NULL} which means no requirements.   
#' @return [\code{\link{Param}}].
#' @name Param
#' @rdname Param
#' @examples 
#' makeNumericParam("x",lower=-1, upper=1)
#' makeNumericVectorParam("x", len=2)
#' makeDiscreteParam("y", values=c("a","b"))
NULL

makeParam = function(id, type, len, lower, upper, values, trafo=NULL, requires=NULL) {
  structure(list(
    id = id,
    type = type,
    len = len,
    lower = lower,
    upper = upper,
    values = values,
    trafo = trafo, 
    requires = requires
  ), class="Param")
}

#' @S3method print Param
print.Param = function(x, ...) {
  type = x$type
  ut = !is.null(x$trafo)
  req = !is.null(x$requires)
  if (type == "numeric")
    catf("Num param '%s'. Constr: %s to %s. Trafo: %s. Requires: %s", x$id, x$lower, x$upper, ut, req) 
  else if (type == "integer")
    catf("Int param '%s'. Constr: %s to %s. Trafo: %s. Requires: %s", x$id, x$lower, x$upper, ut, req) 
  else if (type == "numericvector")
    catf("Num vec param '%s'. Len: %i. Constr: %s to %s. Trafo: %s. Requires: %s", 
      x$id, x$len, collapse(x$lower), collapse(x$upper), ut, req) 
  else if (type == "integervector")
    catf("Int vec param '%s'. Len: %i. Constr: %s to %s. Trafo: %s. Requires: %s", 
      x$id, x$len, collapse(x$lower), collapse(x$upper), ut, req) 
  else if (type == "discrete") 
    catf("Disc param '%s'. Vals: %s. Trafo: %s. Requires: %s", x$id, collapse(names(x$values)), ut, req) 
  else if (type == "discretevector")
    catf("Disc vec param '%s'. Len: %i. Vals: %s. Requires: %s", x$id, x$len, collapse(names(x$values)), req) 
  else if (type == "logical") 
    catf("Log param '%s'. Requires: %s", x$id, req) 
  else if (type == "logicalvector") 
    catf("Log vec param '%s'. Len: %i. Requires: %s", x$id, x$len, req) 
  else if (type == "function")
    catf("Fun param '%s'. Requires: %s", x$id, req) 
  else if (type == "untyped")
    catf("Untyped param '%s'. Trafo: %s. Requires: %s", x$id, ut, req) 
}
