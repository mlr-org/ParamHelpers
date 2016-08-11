context("updateParVals")

test_that("updateParVals works", {
  #simple dependency
  pa = list(a = 1, b = 2, d = 4, e = 5)
  pb = list(a = 0, c = 3)
  ps = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b", requires = quote(a == 1)),
    makeIntegerParam("c"),
    makeIntegerParam("d"),
    makeIntegerParam("e", requires = quote(f == TRUE)),
    makeLogicalParam("f", default = TRUE))
  pc = updateParVals(ps, pa, pb)
  expect_equal(pc, list(a = 0, c = 3, d = 4, e = 5))
  expect_warning(updateParVals(ps, pa, pb, warn = TRUE), "ParamSetting b=2")

  pb2 = list(a = 0, f = FALSE)
  pc2 = updateParVals(ps, pa, pb2)
  expect_equal(pc2, list(a = 0, f = FALSE, d = 4))

  #sanity
  expect_equal(updateParVals(ps, pa, list()), pa)
  expect_equal(updateParVals(ps, list(), pb), pb)

  #chained dependency
  pa = list(a = 1, b = 2, c = 0.5, d = 4)
  pb = list(a = -3)
  ps = makeParamSet(
    makeIntegerParam("a", requires = quote(e == TRUE)),
    makeIntegerParam("b", requires = quote(a == 1)),
    makeNumericParam("c"),
    makeIntegerParam("d", requires = quote(b == 2)),
    makeLogicalParam("e", default = TRUE))
  pc = updateParVals(ps, pa, pb)
  expect_equal(pc, list(a = -3, c = 0.5))

  #chained dependency with defaults #maybe redundanant with pc2?
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L, requires = quote(b == 2)),
    makeIntegerParam("b", default = 2L, requires = quote(c == TRUE)),
    makeLogicalParam("c", default = TRUE))
  pa = list(a = 1, b = 2, c = TRUE)
  pb = list(b = 3)
  pc = updateParVals(ps, pa, pb)
  expect_equal(pc, list(b = 3, c = TRUE))
})
