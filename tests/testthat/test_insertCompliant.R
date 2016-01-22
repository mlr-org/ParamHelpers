context("insertCompliant")

test_that("insertCompliant works", {
  #simple dependency
  pa = list(a = 1, b = 2, d = 4)
  pb = list(a = 0, c = 3)
  ps = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b", requires = quote(a == 1)),
    makeIntegerParam("c"),
    makeIntegerParam("d"))
  pc = insertCompliant(pa, pb, ps)
  expect_equal(pc, list(a = 0, c = 3, d = 4))
  
  #sanity
  expect_equal(insertCompliant(pa, list(), ps), pa)
  expect_equal(insertCompliant(list(), pb, ps), pb)
  
  #chained dependency
  pa = list(a = 1, b = 2, c= 0.5, d = 4)
  pb = list(a = -3)
  ps = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b", requires = quote(a == 1)),
    makeNumericParam("c"),
    makeIntegerParam("d", requires = quote(b == 2)))
  pc = insertCompliant(pa, pb, ps)
  expect_equal(pc, list(a = -3, c = 0.5))
})
