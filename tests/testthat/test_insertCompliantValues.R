context("insertCompliantValues")

test_that("insertCompliantValues works", {
  #simple dependency
  pa = list(a = 1, b = 2, d = 4)
  pb = list(a = 0, c = 3)
  ps = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b", requires = quote(a == 1)),
    makeIntegerParam("c"),
    makeIntegerParam("d"))
  pc = insertCompliantValues(ps, pa, pb)
  expect_equal(pc, list(a = 0, c = 3, d = 4))
  expect_warning(insertCompliantValues(ps, pa, pb, warn = TRUE), "ParamSetting b=2")

  #sanity
  expect_equal(insertCompliantValues(ps, pa, list()), pa)
  expect_equal(insertCompliantValues(ps, list(), pb), pb)

  #chained dependency
  pa = list(a = 1, b = 2, c = 0.5, d = 4)
  pb = list(a = -3)
  ps = makeParamSet(
    makeIntegerParam("a"),
    makeIntegerParam("b", requires = quote(a == 1)),
    makeNumericParam("c"),
    makeIntegerParam("d", requires = quote(b == 2)))
  pc = insertCompliantValues(ps, pa, pb)
  expect_equal(pc, list(a = -3, c = 0.5))
})
