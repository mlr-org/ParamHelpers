context("getRequirements")

test_that("getRequirements", {
  ps = makeParamSet()
  # FIXME: BBmisc::namedList still buggy, only return list()
  xs = setNames(list(), character(0L))
  expect_equal(getRequirements(ps), xs)

  r1 = quote(a == 1)
  r2 = quote(a == 2)
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L),
    makeIntegerParam("b", default = 1L, requires = r1),
    makeNumericParam("c", default = 1L, requires = r2)
  )

  expect_equal(getRequirements(ps, remove.null = TRUE), list(b = r1, c = r2))
  expect_equal(getRequirements(ps, remove.null = FALSE), list(a = NULL, b = r1, c = r2))
})

