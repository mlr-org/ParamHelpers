context("isNumericNoRequires")

test_that("isNumericNoRequires", {
  ps = makeParamSet()
  expect_true(isNumericNoRequires(ps))
  
  ps = makeParamSet(
    makeNumericParam("x", lower=1, upper=2)
  )
  expect_true(isNumericNoRequires(ps))
  
  ps = makeParamSet(
    makeDiscreteParam("x", values=1:2)
  )
  expect_false(isNumericNoRequires(ps))
  
  ps = makeParamSet(
    makeDiscreteParam("x", values=1:2),
    makeNumericParam("y", lower=1, upper=2, requires=quote(x == 1))
  )
  expect_false(isNumericNoRequires(ps))
})
