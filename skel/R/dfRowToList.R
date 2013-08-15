#' Convert a data.frame row to list of parameter-value-lists.
#'
#' @param df [\code{data.frame}]\cr
#'   Data.frame, probably from OptPatDF.
#'   Columns are assumed to be in the same order as par.set. 
#' @param par.set [\code{\link{ParamSet}}]\cr
#'   Parameter set.
#' @param i [\code{integer(1)}]\cr
#'   Row index.
#' @return [\code{list}]. Named by parameter ids. 
#' @export
dfRowToList = function(df, par.set, i, remove.missing.values=FALSE) {
  for (j in 1:ncol(df)) {
    if (is.factor(df[,j]))
      df[,j] = as.character(df[,j])
  }
  dfRowToList2(df, par.set, i, remove.missing.values)
}

dfRowToList2 = function(df, par.set, i, remove.missing.values=FALSE) {
  df = df[i,,drop=FALSE]
  pars = par.set$pars
  col = 0
  x = list()
  for (i in 1:length(pars)) {
    p = pars[[i]]
    cc = rev(col)[1]
    if (p$type %in% c("numericvector", "integervector", "discretevector", "logicalvector")) 
      col = (cc + 1) : (cc + p$len)   
    else 
      col = cc + 1  
    entry = df[,col]
    if (all(is.na(entry))) {
      # missing value due to unsatified dependencies / requires
      x[[p$id]] = NA
    } else {
      # convert from to df to vector and remove names in the next 2 
      if (p$type %in% c("numeric", "numericvector")) 
        x[[p$id]] = as.numeric(entry)
      else if (p$type %in% c("integer", "integervector")) 
        x[[p$id]] = as.integer(entry)
      else if (p$type %in% c("logical", "logicalvector"))
        x[[p$id]] = as.logical(entry)
      else if (p$type %in% c("discrete", "discretevector"))
        x[[p$id]] = discreteNameToValue(p, as.character(entry))
    }
  }
  if (remove.missing.values) {
    j = sapply(x, isScalarNA)
    x[j] = NULL
  }
  return(x)
}

#' @export
#' @rdname dfRowToList
dfRowsToList = function(df, par.set, remove.missing.values=FALSE) {
  for (j in 1:ncol(df)) {
    if (is.factor(df[,j]))
      df[,j] = as.character(df[,j])
  }
  lapply(1:nrow(df), dfRowToList2, df=df, par.set=par.set, remove.missing.values=remove.missing.values)
}
