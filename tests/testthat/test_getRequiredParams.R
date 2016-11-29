context("getRequiredParams")

test_that("getRequiredParams", {
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L),
    makeIntegerParam("b", default = 1L, requires = quote(a == 1)),
    makeIntegerParam("c", default = 1L, requires = quote(a == 2 && b == 1))
  )
  
  
  
  expect_equal(getParamIds(getRequiredParams(ps)), c("a", "b"))
  ps2 = filterParams(ps, ids = c("b", "c"))
  expect_equal(getParamIds(getRequiredParams(ps2)), c("b"))
  
  ps2 = filterParams(ps, ids = c("c"))
  expect_equal(getRequiredParams(ps2), makeParamSet())
  
  ps2 = filterParams(ps, ids = character(0))
  expect_equal(getRequiredParams(ps2), makeParamSet())
})
