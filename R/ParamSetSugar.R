#' defined to avoid problems with the static type checker
#' @export
discrete = list

#' defined to avoid problems with the static type checker
#' @export
funct = list

#' defined to avoid problems with the static type checker
#' @export
untyped = list


#' @title Turn the argument list into a \code{ParamSet} of \code{LearnerParam}s
#'
#' @description
#'
#' \code{pSS}, short for \code{paramSetSugar}, is a shorthand API for \code{ParamHelpers}
#' which enables entry of \code{\link{ParamSet}}s in short form. It behaves similarly to
#' \code{\link{makeParamSet}}, but instead of having to construct each parameter individually,
#' the parameters can be given in shorthand form with a convenient syntax, making use of R's
#' nonstandard evaluation.
#'
#' The arguments are of the form
#'
#' \code{name = default: type range [^ dimension] [settings]}.
#'
#' \code{name} is any valid R identifier name.
#'
#' \dQuote{= default} Determines the 'default' setting
#' in \dQuote{makeXXXLearnerParam}. Note that this is different from an R function parameter
#' default value, in that it serves only as information to the user and does not set the
#' parameter to this value if it is not given. To define `no default`, use NA or
#' leave the \dQuote{= default} part out. Leaving it out can cause problems when R's static
#' type checker verifies a package, so this is *only* recommended for interactive sessions
#' and top-level applications! (To actually set a parameter default to NA, put it in parentheses)
#'
#' \code{type} is one of
#' \dQuote{integer}, \dQuote{numeric}, \dQuote{logical}, \dQuote{discrete}, \dQuote{funct}, \dQuote{character}, \dQuote{untyped}
#'
#' \code{range} is absent for type \dQuote{logical}, \dQuote{funct}, \dQuote{character}, or \dQuote{untyped}. For \dQuote{discrete},
#' it is either \code{[valuelist]} with \code{valuelist} evaluating to a list,
#' or of the form \code{[value1, value2, ...]}, creating a discrete parameter of character
#' or numeric values according to \code{value1},
#' \code{value2} etc. If \code{type} is one of \dQuote{integer} or \dQuote{numeric},
#' \code{range} is of the form \code{[lowBound, upBound]}, where \code{lowBound}
#' and \code{upBound} must either be numerical (or integer) values indicating the
#' lower and upper bound, or may be missing (indicating the absence of a bound). To indicate
#' an exclusive bound, prefix the values with a tilde (\dQuote{~}). For a \dQuote{numeric} variable, to
#' indicate an unbounded value which may not be infinite, you can use \code{~Inf} resp \code{~-Inf},
#' or use tilde-dot (\dQuote{~.}).
#'
#' \code{^ dimension} may be absent, resulting in a normal \code{\link{LearnerParam}}, or present,
#' resulting in a \code{VectorLearnerParam}. Note that a one-dimensional \code{VectorLearnerParam}
#' is distinct from a normal \code{\link{LearnerParam}}.
#'
#' \code{settings} may be a collection of further settings to supply to \code{makeXXXLearnerParam}
#' and is optional. To specify a series of settings, put in double square brackets (\code{[[}, \code{]]}),
#' and comma-separate settings if more than one is present.
#'
#' This makes definition of \code{\link{ParamSet}}s shorter and more readable.
#'
#' The difference between \code{pSS} and \code{pSSLrn} is only in the default value of \code{.pss.learner.params}
#' being \code{FALSE} for the former and \code{TRUE} for the latter.
#'
#' @param ... Parameters, see description.
#' @param .pss.learner.params [\code{logical}]\cr
#'   Whether to create \code{\link{LearnerParam}} instead of \code{\link{Param}} objects.
#'   Default is \code{TRUE} for \code{pSSLrn} and \code{FALSE} for \code{pSS}.
#' @param .pss.env [\code{environment}]\cr
#'   Which environment to use when evaluating expressions. Defaults to the calling
#'   function's frame.
#'
#' @examples
#' pSSLrn(a = NA: integer [~0, ]^2 [[requires = expression(b != 0)]],
#'        b = -10: numeric [~., 0], c: discrete [x, y, 1])
#' # is equivalent to
#' makeParamSet(
#'     makeIntegerVectorLearnerParam('a', len = 2, lower = 1,  # note exclusive bound
#'          upper = Inf, requires = expression(b != 0)),
#'     makeNumericLearnerParam('b', lower = -Inf, upper = 0,
#'          allow.inf = FALSE, default = -10),  # note infinite value is prohibited.
#'     makeDiscreteLearnerParam('c', values = list(x = "x", y = "y", `1` = 1))
#' )
#'
#'
#' @export
pSS = function(..., .pss.learner.params = FALSE, .pss.env = parent.frame()) {
  promises = substitute(pSS(...))  # match.call doesn't work with indirect calls.
  promises[[1]] = NULL
  allparams = lapply(seq_along(promises), function(paridx) {
    thispar = promises[[paridx]]
    name = coalesce(names(promises)[paridx], "")
    parseSingleParameter(name, thispar, .pss.learner.params, .pss.env)
  })
  makeParamSet(params = allparams)
}

#' @rdname pSS
#' @export
pSSLrn = function(..., .pss.learner.params = TRUE, .pss.env = parent.frame()) {
  pSS(..., .pss.learner.params = .pss.learner.params, .pss.env = .pss.env)
}

### Auxiliary functions

# formerr: Give informational error about malformed parameter
formerr = function(pstring, specific) {
    stopf("Parameter '%s' must be of the form\n%s\n%s", pstring,
          "NAME = DEFAULT: TYPE [RANGE] [^ DIMENSION] [SETTINGS]",
          specific)
}

# get the makeXXXParam function appropriate for the type and vector-ness
getConstructor = function(type, is.learner, is.vector) {
  normal.const = list(numeric = makeNumericParam,
                     integer = makeIntegerParam,
                     logical = makeLogicalParam,
                     discrete = makeDiscreteParam,
                     funct = makeFunctionParam,
                     character = makeCharacterParam,
                     untyped = makeUntypedParam)
  vector.const = list(numeric = makeNumericVectorParam,
                     integer = makeIntegerVectorParam,
                     logical = makeLogicalVectorParam,
                     discrete = makeDiscreteVectorParam,
                     character = makeCharacterVectorParam)
  normlrn.const = list(numeric = makeNumericLearnerParam,
                      integer = makeIntegerLearnerParam,
                      logical = makeLogicalLearnerParam,
                      discrete = makeDiscreteLearnerParam,
                      funct = makeFunctionLearnerParam,
#                      character = makeCharacterLearnerParam,
                      untyped = makeUntypedLearnerParam)
  vectlrn.const = list(numeric = makeNumericVectorLearnerParam,
                      integer = makeIntegerVectorLearnerParam,
                      logical = makeLogicalVectorLearnerParam,
                      discrete = makeDiscreteVectorLearnerParam)
#                      character = makeCharacterVectorLearnerParam,
    if (is.vector) {
    if (is.learner) {
      vectlrn.const[[type]]
    } else {
      vector.const[[type]]
    }
  } else {
    if (is.learner) {
      normlrn.const[[type]]
    } else {
      normal.const[[type]]
    }
  }
}

### parameter parsing sub-functions

# parseDimension: parse the '^n' part indicating a vector
parseDimension = function(pdeco, pstring, pss.env) {
  if (is.recursive(pdeco[[3]]) && identical(pdeco[[3]][[1]], quote(`[[`))) { # settings
    exponent = pdeco[[3]][[2]]
    # remove '^..' part, but keep settings part
    new.pdeco = pdeco[[3]]
    new.pdeco[[2]] = pdeco[[2]]
    pdeco = new.pdeco
  } else {
    exponent = pdeco[[3]]
    pdeco = pdeco[[2]]  # remove '^...' part
  }
  len = eval(exponent, envir = pss.env)
  if (!is.numeric(len) && !is.na(len)) {
    formerr(pstring, sprintf("`^` found, but exponent %s did not eval to numeric.", deparseJoin(exponent)))
  }
  list(pdeco = pdeco, len = len)
}

# parseDiscrete: parse the range part of a discrete parameter
parseDiscrete = function(pdeco, pstring, pss.env) {
  if (length(pdeco) == 2) {
    # only one value given -> interpret it as expression that gives the values list
    return(eval(pdeco[[2]], envir = pss.env))
  } else if (all(names(pdeco) == "")) {
    # succession of names of the kind (a, b, c, 1, 2, 3) -> turn it into a list
    vallist = as.list(pdeco)
    vallist[[1]] = NULL
    vallist = lapply(vallist, function(item) {
      if (is.name(item)) {
        as.character(item)
      } else if (is.numeric(item)) {
        item
      } else {
        formerr(pstring, sprintf("value list %s invalid", deparseJoin(pdeco)))
      }
    })
    names(vallist) = sapply(vallist, as.character)
    return(vallist)
  } else {
    pdeco[[1]] = quote(list)
    return(eval(as.call(pdeco), envir = pss.env))
  }
}

# parseNumeric: parse the range part of a numeric / integer parameter
parseNumeric = function(pdeco, ptype, pstring, pss.env) {
  if (length(pdeco) != 3) {
    formerr(pstring, "invalid numeric / integer range")
  }
  quasi.inf = .Machine$double.xmax
  parse.bound = function(expr, lower) {
    if (is.recursive(expr) && identical(expr[[1]], quote(`~`))) {
      if (length(expr) == 2 &&  identical(expr[[2]], quote(`.`))) {
        value = ifelse(lower, -Inf, Inf)
      } else {
        value = eval(expr[[2]], envir = pss.env)
      }
      if (is.infinite(value)) {
        if (ptype == "integer") {
          formerr(pstring, '"."-bounds (unbounded but excluding "Inf") are only allowed for "numeric" variables.')
        }
        if ((value < 0) == lower) {
          value = ifelse(lower, -quasi.inf, quasi.inf)
        }
      } else if (ptype == "integer") {
        value = value + ifelse(lower, 1, -1)
      } else {
        if (value == 0) {
          value = value + .Machine$double.xmin * ifelse(lower, 1, -1)
        } else {
          epsilon = ifelse(lower == (value > 0), .Machine$double.eps, -.Machine$double.neg.eps)
          value = value + epsilon * value
        }
      }
    } else if (identical(expr, substitute())) {
      if (length(expr) > 1) {
        formerr(pstring, "invalid numeric / integer range")
      }
      value = ifelse(lower, -Inf, Inf)
    } else {
      value = eval(expr, envir = pss.env)
    }
  }
  lower.bound = parse.bound(pdeco[[2]], TRUE)
  upper.bound = parse.bound(pdeco[[3]], FALSE)

  allow.inf = TRUE
  if (lower.bound == -quasi.inf || upper.bound == quasi.inf) {
    if (lower.bound != -Inf && upper.bound != Inf) {
      # no true 'Inf' occurs, so we can translate quasi.inf to Inf and
      # instead use the 'allow.inf' parameter
      if (lower.bound == -quasi.inf) {
        lower.bound = -Inf
      }
      if (upper.bound == quasi.inf) {
        upper.bound = Inf
      }
      allow.inf = FALSE
    }
  }
  rl = list(lower = lower.bound, upper = upper.bound)
  if (ptype == "numeric") {
    rl$allow.inf = allow.inf
  }
  rl
}

### parsing single parameter
# this function does the heavy lifting:
# it takes the name and expression of a given parameter and returns
# the constructed ParamSet.
parseSingleParameter = function(name, thispar, is.learner, pss.env) {
  constructor.params = list()
  additional.settings = list()
  is.vector = FALSE
  pstring = deparseJoin(thispar)
  if (name != "") {
    pstring = paste(name, "=", pstring)
  }

  if (!identical(thispar[[1]], quote(`:`))) {
    formerr(pstring, "`:` was missing or at unexpected position.")
  }

  if (name == "") {  # no default
    constructor.params$id = as.character(thispar[[2]])
  } else {
    constructor.params$id = name
    if (!identical(thispar[[2]], NA)) {
      constructor.params["default"] = list(eval(thispar[[2]], envir = pss.env))
    }
  }

  pdeco = thispar[[3]]
  if (is.recursive(pdeco) && identical(pdeco[[1]], quote(`^`))) { # dimension
    rl = parseDimension(pdeco, pstring, pss.env)
    pdeco = rl$pdeco
    constructor.params$len = rl$len
    is.vector = TRUE
  }
  if (is.recursive(pdeco) && identical(pdeco[[1]], quote(`[[`))) {  # settings
    additional.settings = as.list(pdeco)
    additional.settings[[1]] = NULL  # delete `[[`
    additional.settings[[1]] = NULL  # delete part before `[[`
    additional.settings = lapply(additional.settings, function(x) eval(x, envir = pss.env))
    pdeco = pdeco[[2]]
  }
  if (!is.recursive(pdeco)) {
    pdeco = list(pdeco)  # so we can access [[1]]
  }
  if (identical(pdeco[[1]], quote(`[`))) {
    pdeco[[1]] = NULL
  } else if (!(
    identical(pdeco[[1]], quote(logical)) || identical(pdeco[[1]], quote(funct)) ||
    identical(pdeco[[1]], quote(character)) || identical(pdeco[[1]], quote(untyped)))) {
    # the only type that may have no range attached is `logical`
    if (as.character(pdeco[[1]]) %in% c("integer", "numeric", "logical", "discrete")) {
      formerr(pstring, sprintf("range is missing"))
    } else {
      formerr(pstring, sprintf("range must be indicated using square brackets"))
    }
  }
  ptype = as.character(pdeco[[1]])
  if (!ptype %in% c("integer", "numeric", "logical", "discrete", "funct", "character", "untyped")) {
    formerr(pstring, sprintf("Unknown parameter type %s", deparseJoin(pdeco[[1]])))
  }

  # check sanity of 'range' part
  needspostfix = ptype %in% c("discrete", "numeric", "integer")
  if (!needspostfix && length(pdeco) > 1) {
    formerr(pstring, sprintf("Logical parameter with unexpected postfix: %s", deparseJoin(pdeco)))
  }
  if (needspostfix && length(pdeco) == 1) {
    formerr(pstring, sprintf("Parameter of type '%s' needs range postfix.", ptype))
  }

  # interpret range of discrete parameters
  if (ptype == "discrete") {
    constructor.params$values = parseDiscrete(pdeco, pstring, pss.env)
  }
  if (ptype %in% c("numeric", "integer")) {
    constructor.params = insert(constructor.params, parseNumeric(pdeco, ptype, pstring, pss.env))
  }
  constructor.params = insert(constructor.params, additional.settings)
  do.call(getConstructor(ptype, is.learner, is.vector), constructor.params, quote = TRUE)
}

# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

