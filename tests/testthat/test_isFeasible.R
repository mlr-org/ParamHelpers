context("isFeasible")

test_that("isFeasible ParamSet", {
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L),
    makeDiscreteParam("b", values = list('0' = "NIL", '1' = "ONE", 'f' = function(x) x + 1)),
    makeIntegerParam("c", default = 1L, requires = quote(a == 1)),
    makeIntegerParam("d", default = 1L, requires = quote(a == 2))
  )
  expect_true(isFeasible(ps, list(a = 1, b = function(x) x+1, c = 1, d = NA)))
  expect_true(isFeasible(ps, list(a = 1, b = function(x) x+1, c = 1), filter = TRUE))
  expect_true(isFeasible(ps, list(a = 2, b = function(x) x+1, d = 1), filter = TRUE))
  expect_error(isFeasible(ps, list(a = 1, c = 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1), filter = TRUE), "only works with named input")
  expect_error(isFeasible(ps, list(a = 1, zzz = 1), filter = TRUE), "zzz")
  expect_false((res = isFeasible(ps, list(a = 1, c = "1"), filter = TRUE)))
  expect_match(attr(res, "warning"), "c=1 does not meet constraints")
  expect_error(isFeasible(ps, list(c = 2), filter = TRUE), "needed for requirements: a")
  expect_false((res = isFeasible(ps, list(a = 2, c = 2), filter = TRUE)))
  expect_match(attr(res, "warning"), "Param c=2 is set but does not meet requirements")
  expect_true((res = isFeasible(ps, list(a = 2, c = NA), filter = TRUE)))
  expect_true(isFeasible(ps, list(c = 2), filter = TRUE, use.defaults = TRUE))
})
