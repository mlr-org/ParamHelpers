test_that("getRequiredParamNames", {
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L),
    makeIntegerParam("b", default = 1L, requires = quote(a == 1)),
    makeIntegerParam("c", default = 1L, requires = quote(a == 2 && b == 1))
  )

  expect_equal(getRequiredParamNames(ps), c("a", "b"))
  ps2 = filterParams(ps, ids = c("b", "c"))
  expect_equal(getRequiredParamNames(ps2), c("a", "b"))

  ps2 = filterParams(ps, ids = c("c"))
  expect_equal(getRequiredParamNames(ps2), c("a", "b"))

  ps2 = filterParams(ps, ids = character(0))
  expect_equal(getRequiredParamNames(ps2), character(0))
})
