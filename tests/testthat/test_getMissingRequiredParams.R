context("getMissingRequiredParams")

test_that("getMissingRequiredParams", {
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L),
    makeIntegerParam("b", default = 1L, requires = quote(a == 1)),
    makeIntegerParam("c", default = 1L, requires = quote(a == 2 && b == 1))
  )

  expect_equal(getMissingRequiredParams(ps, getParamIds(ps)), character(0))
  ps2 = filterParams(ps, ids = c("b", "c"))
  expect_equal(getMissingRequiredParams(ps2, getParamIds(ps2)), "a")

  ps2 = filterParams(ps, ids = c("c"))
  expect_equal(getMissingRequiredParams(ps2, getParamIds(ps2)), c("a", "b"))

  ps2 = filterParams(ps, ids = character(0))
  expect_equal(getMissingRequiredParams(ps2, getParamIds(ps2)), character(0))
})
