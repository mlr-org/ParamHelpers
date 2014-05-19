context("getDefaults")

test_that("getDefaults", {
  ps = makeParamSet()
  expect_equal(getDefaults(ps), list())

  ps = makeParamSet(
    makeNumericParam("u", default = 2),
    makeIntegerParam("v"),
    makeUntypedParam("w", default = iris)
  )
  expect_equal(getDefaults(ps), list(u = 2, w = iris))

  ps = makeParamSet(
    makeIntegerParam("v")
  )
  expect_equal(getDefaults(ps), list())
})

