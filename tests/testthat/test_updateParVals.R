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
})
