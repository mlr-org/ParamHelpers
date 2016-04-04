library(cluster)

generateDesignOversampling = function(
  n = 10L, par.set, trafo = FALSE, oversampling = 20L) {
  
  design = convertListOfRowsToDataFrame(sampleValues(par.set, oversampling * n,
    trafo = trafo))
  dists = daisy(x = design, metric = "gower")
  
  # I know, we double the memory requirement here, but working with a matrix
  # is much much easier ...
  dists = as.matrix(dists)
  diag(dists) = Inf
  
  while (nrow(design) > n) {
    # Which observations should be removed? Sample between the candidates
    remove.ind = sample(which(apply(dists == min(dists), 1, any)), 1)
    # Remove it
    design = design[-remove.ind, ]
    dists = dists[-remove.ind, -remove.ind]
  }
  
  return(design)
}



par.set =   makeParamSet(
  makeDiscreteParam("selected.learner", values = c("a", "b", "c")),
  
  makeIntegerParam("classif.randomForest.mtry",
    lower = 15, upper = 45,
    requires = quote(selected.learner == "a")),
  makeIntegerParam("classif.randomForest.nodesize", lower = 1L, upper = 120,
    requires = quote(selected.learner == "a")),
  
  makeNumericParam("classif.svm.cost", lower = -15L, upper = 15L, trafo = function(x) 2^x,
    requires = quote(selected.learner == "b")),
  makeDiscreteParam("classif.svm.kernel", values = c("linear", "radial"),
    requires = quote(selected.learner == "b")),
  makeNumericParam("classif.svm.gamma", lower = -15L, upper = 15L, trafo = function(x) 2^x,
    requires = quote(selected.learner == "b" & classif.svm.kernel == "radial")),
  
  makeIntegerParam("classif.kknn.k", lower = 1L, upper = 21L,
    requires = quote(selected.learner == "c"))
)

des = generateDesignOversampling(20, par.set)
