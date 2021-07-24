test_that("hasExpression.Param", {
  expect_true(!hasExpression(obj = makeDiscreteParam("u", values = 1:2)))
  expect_true(hasExpression(obj = makeDiscreteParam("v", values = expression(n * p))))
  expect_true(!hasExpression(obj = makeNumericParam("w", lower = -Inf)))
  expect_true(hasExpression(obj = makeNumericParam("x", lower = expression(ceiling(n)))))
})


test_that("hasExpression.ParamSet", {
  ps1 = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 2),
    makeNumericParam("y", lower = 1, upper = 10)
  )
  ps2 = makeParamSet(
    makeNumericLearnerParam("x", lower = 1, upper = 2),
    makeNumericLearnerParam("y", lower = 1, upper = expression(p))
  )
  ## providing the correct key is ok
  ps3 = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 2),
    makeNumericParam("y", lower = 1, upper = expression(a)),
    keys = "a"
  )
  # providing the wrong key will result in an error
  expect_error(makeParamSet(
    makeNumericParam("x", lower = 1, upper = 2),
    makeNumericParam("y", lower = 1, upper = expression(a)),
    keys = "b"
  ))
  expect_true(!hasExpression(ps1))
  expect_true(hasExpression(ps2))
  expect_true(hasExpression(ps3))
})

test_that("hasExpression.list", {
  par.vals = list(
    makeLogicalParam("x", default = expression(a == 3)),
    makeNumericParam("y", lower = 1, upper = expression(a))
  )
  expect_true(hasExpression(par.vals))
})
