library(methods)
library(devtools)
library(testthat)
library(BBmisc)
library(soobench)
library(lhs)

if (interactive()) {
  load_all(".", reset=TRUE)
} else {
  library(ParamHelpers)
}
test_dir("tests/testthat")


