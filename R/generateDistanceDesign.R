generateDistanceDesign = function(n = 10L, k = 10^3, restarts = 1L, performance = mean, par.set, trafo = FALSE) {
  
  results = list()
  
  for (r in seq_len(restarts)) {
    des = generateRandomDesign(n = k, par.set = par.set, trafo = trafo)
    
    dist = gower.dist(des)
    diag(dist) = Inf
    rownames(dist) = 1:k
    colnames(dist) = 1:k
    
    for (i in n:(k-1)) {
      del = sample(as.numeric(which(dist == min(dist), arr.ind = TRUE)), 1)
      dist = dist[-del, -del]
    }
    
    res = des[as.numeric(colnames(dist)), ]
    results[[r]] = list(
      res = res,
      perf = performance(gower.dist(res))
    )
  }
  
  best = which.min(sapply(results, function(r) r$perf))
  results[[best]]$res
}