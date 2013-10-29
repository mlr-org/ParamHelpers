library(devtools)
library(testthat)
library(BBmisc)

load_all(".", reset=TRUE)
source("R/generateDesign.R")

# enter some code for manual testing here

ps1 = makeParamSet(
  makeNumericVectorParam("x", len=2, lower=0, upper=1),
  makeIntegerVectorParam("y", len=3, lower=10, upper=20),
  makeLogicalVectorParam("z", len=2),
  makeDiscreteVectorParam("w", len=3, values=c("a", "b", "c"))
)

ps2 = makeParamSet(
  makeNumericParam("x", lower=0, upper=1),
  makeNumericVectorParam("y", len=2, lower=5, upper=6, requires=quote(x > 0.5)),
  makeLogicalParam("v", requires=quote(x < 0.5))
)


ps3 = makeParamSet(
  makeNumericVectorParam("x", len=2, lower=0, upper=1, trafo=function(x) 2^x),
  makeIntegerVectorParam("y", len=3, lower=10, upper=20),
  makeLogicalVectorParam("z", len=2),
  makeDiscreteVectorParam("z2", len=3, values=c("a", "b", "c"))
)

# des = generateDesign(4, ps1)
# print(des)

measure = function(ps, n) {
  st1 = system.time({des1 = generateDesign(n, ps)})
  print(st1[3])
  st2 = system.time({des2 = generateDesign2(n, ps)})
  print(st2[3])
  messagef("speedup = %.3g", st2[3] / st1 [3])
}

measure(ps1, 10000)
measure(ps2, 10000)
measure(ps3, 10000)
