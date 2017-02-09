context("isSimpleNumeric")

test_that("isSimpleNumeric", {
  ps = makeParamSet()
  expect_true(isSimpleNumeric(ps))
  
  ps = makeParamSet(
    makeNumericParam("x", lower=1, upper=2)
  )
  expect_true(isSimpleNumeric(ps))
  
  ps = makeParamSet(
    makeDiscreteParam("x", values=1:2)
  )
  expect_false(isSimpleNumeric(ps))
  
  ps = makeParamSet(
    makeDiscreteParam("x", values=1:2),
    makeNumericParam("y", lower=1, upper=2, requires=quote(x == 1))
  )
  expect_false(isSimpleNumeric(ps))
})
