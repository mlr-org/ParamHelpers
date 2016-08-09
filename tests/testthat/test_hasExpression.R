context("hasExpression")

test_that("hasExpression.Param", {
  expect_true(!hasExpression(par = makeDiscreteParam("u", values = 1:2)))
  expect_true(hasExpression(par = makeDiscreteParam("v", values = expression(n * p))))
  expect_true(!hasExpression(par = makeNumericParam("w", lower = -Inf)))
  expect_true(hasExpression(par = makeNumericParam("x", lower = expression(ceiling(n)))))
})


test_that("hasExpression.ParamSet", {
  ps1 = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 2),
    makeNumericParam("y", lower = 1, upper = 10)
  )
  ps2 = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 2),
    makeNumericParam("y", lower = 1, upper = expression(a))
  )
  expect_true(!hasExpression(ps1))
  expect_true(hasExpression(ps2))
})