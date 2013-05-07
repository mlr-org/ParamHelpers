library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all("skel", reset=TRUE)
} else {
  library(ParamHelpers)  
}
test_dir("skel/inst/tests")

