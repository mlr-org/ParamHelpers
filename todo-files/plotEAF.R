


plotEAF = function(opt.paths, y.names = NULL, ...) {
  requirePackages("eaf", why = "plotEAF")
  assertList(opt.paths, min.len = 1L, types = "list", names = "unique")
  algos = names(opt.paths)

  data = data.frame()
  for (i in seq_along(algos)) {
    a = algos[i]
    runs = opt.paths[[i]]
    assertList(runs, types = "OptPath", min.len = 1L)
    fronts = lapply(seq_along(runs), function(j) {
      run = runs[[j]]
      f = as.data.frame(getOptPathParetoFront(run))
      cns = colnames(f)
      if (length(cns) != 2L)
        stop("Must always have 2 objectives in opt path. But found: %i", length(cns))
      if (is.null(y.names) && i == 1L && j == 1L)
        y.names <<- cns
      if (!all(y.names == cns))
        stop("Must always have the same 2 objectives in opt path: %s. But found: %s",
          collapse(y.names), collapse(cns))
      cbind(f, .algo = a, .repl = j)
    })
    fronts = do.call(rbind, fronts)
    data = rbind(data, fronts)
  }
  yn1 = y.names[1L]; yn2 = y.names[2L]
  f = as.formula(sprintf("%s + %s ~ .repl", yn1, yn2))
  eafplot(f, data = data, groups = .algo, percentiles = c(0, 50, 100),
    xlab = yn1, ylab = yn2, ...)
  return(data)
}


