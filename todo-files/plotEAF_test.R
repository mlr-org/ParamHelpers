setwd("/home/bischl/cos/mlrMBO/")

library(BatchExperiments)
library(checkmate)

source("/home/bischl/cos/ParamHelpers/R/plotEAF.R")


# reg = loadRegistry("/home/bischl/cos/mco_bench-files/", work.dir = "~/cos/mlrMBO/")

j1 = findExperiments(reg, prob.pattern = "zdt1_5D2M", algo.pattern = "nsga2", algo.pars = (budget == "normal"))
j2 = findExperiments(reg, prob.pattern = "zdt1_5D2M", algo.pattern = "nsga2", algo.pars = (budget == "10fold"))

ops = list()
ops[["ns1"]] = extractSubList(loadResults(reg, j1), "opt.path", simplify = FALSE)
ops[["ns2"]] = extractSubList(loadResults(reg, j2), "opt.path", simplify = FALSE)


d = plotEAF(ops)




