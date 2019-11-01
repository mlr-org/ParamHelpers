#' @title Get parameter subset of only certain parameters.
#'
#' @description
#' Parameter order is not changed. It is possible to filter via multiple
#' arguments, e.g., first filter based on id, then the type and lastly tunable.
#' The order in which the filters are executed is always fixed (id > type >
#' tunable).
#'
#' @template arg_parset
#' @param ids (`NULL` | `character`)\cr
#'   Vector with id strings containing the parameters to select. Has to be a
#'   subset of the parameter names within the parameter set.
#'   Per default (`ids = NULL`) no filtering based on names is done.
#' @param type (`NULL` | `character`)\cr
#'   Vector of allowed types, subset of: \dQuote{numeric}, \dQuote{integer},
#'   \dQuote{numericvector}, \dQuote{integervector}, \dQuote{discrete},
#'   \dQuote{discretevector}, \dQuote{logical}, \dQuote{logicalvector},
#'   \dQuote{character}, \dQuote{charactervector}, \dQuote{function},
#'   \dQuote{untyped}.
#'   Setting `type = NULL`, which is the default, allows the consideration of all types.
#' @param tunable (`logical`)\cr
#'   Vector of allowed values for the property `tunable`. Accepted arguments are
#'   `TRUE`, `FALSE` or `c(TRUE, FALSE)`. The default is `c(TRUE, FALSE)`, i.e.
#'   none of the parameters will be filtered out.
#' @param check.requires (`logical(1)`)\cr
#'   Toggle whether it should be checked that all requirements in the
#'   (ParamSet()) are still valid after filtering or not. This check is done
#'   after filtering and will throw an error if those Params are filtered which
#'   other Params need for their requirements. Default is `FALSE`.
#' @return [ParamSet()].
#' @examples
#' ps = makeParamSet(
#'   makeNumericParam("u", lower = 1),
#'   makeIntegerParam("v", lower = 1, upper = 2),
#'   makeDiscreteParam("w", values = 1:2),
#'   makeLogicalParam("x"),
#'   makeCharacterParam("s"),
#'   makeNumericParam("y", tunable = FALSE)
#' )
#'
#' # filter for numeric and integer parameters
#' filterParams(ps, type = c("integer", "numeric"))
#'
#' # filter for tunable, numeric parameters
#' filterParams(ps, type = "numeric", tunable = TRUE)
#'
#' # filter for all numeric parameters among "u", "v" and "x"
#' filterParams(ps, type = "numeric", ids = c("u", "v", "x"))
#' @export
filterParams = function(par.set, ids = NULL, type = NULL, tunable = c(TRUE, FALSE), check.requires = FALSE) {
  # if (!is.null(par.set$forbidden))
  # stopf("Operation not allowed for param set with forbidden region currently!")
  if (!is.null(ids)) {
    assertSubset(ids, names(par.set$pars))
    par.set$pars = Filter(function(p) p$id %in% ids, par.set$pars)
  }
  if (!is.null(type)) {
    assertSubset(type, getTypeStringsAll())
    par.set$pars = Filter(function(p) p$type %in% type, par.set$pars)
  }
  assertLogical(tunable, min.len = 1L, max.len = 2L, unique = TRUE)
  par.set$pars = Filter(function(p) p$tunable %in% tunable, par.set$pars)
  if (check.requires) {
    # find all vars which are in each params requirements which are not part of the param.set
    missing.vars = setdiff(getRequiredParamNames(par.set), getParamIds(par.set))
    if (length(missing.vars) > 0) {
      stopf("Params %s filtered but needed for requirements of present Params", collapse(missing.vars))
    }
  }
  return(par.set)
}


#' @template arg_include_int
#' @rdname filterParams
#' @export
filterParamsNumeric = function(par.set, ids = NULL, tunable = c(TRUE, FALSE), include.int = TRUE) {
  filterParams(par.set, ids = ids, tunable = tunable,
    type = getTypeStringsNumeric(include.int = include.int))
}

#' @template arg_include_logical
#' @rdname filterParams
#' @export
filterParamsDiscrete = function(par.set, ids = NULL, tunable = c(TRUE, FALSE), include.logical = TRUE) {
  filterParams(par.set, ids = ids, tunable = tunable,
    type = getTypeStringsDiscrete(include.logical = include.logical))
}
