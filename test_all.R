library(methods)
library(devtools)
library(testthat)
library(BBmisc)

if (interactive()) {
  load_all(".", reset=TRUE)
} else {
  library(ParamHelpers)  
}
test_dir("inst/tests")


