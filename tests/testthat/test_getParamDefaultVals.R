context("getParamDefaultVals")

test_that("getParamDefaultVals", {
  ps = makeParamSet(
    makeDiscreteParam("x", values = c("a", "b"), default = "a"),
    makeNumericVectorParam("y", len=2),
    makeIntegerParam("z", default = 99)
  )

  expect_equal(
    getParamDefaultVals(ps),
    list(x = "a", y = NULL, z = 99)
  )
})

