test_that("no changes", {
  ps = makeParamSet()
  expect_identical(evaluateParamExpressions(ps), ps)
  expect_identical(evaluateParamExpressions(ps, dict = list(p = 3)), ps)

  ps = makeParamSet(
    makeNumericParam("u", lower = 3),
    makeIntegerParam("v", lower = 1, upper = 9),
    makeDiscreteParam("w", default = "b", values = c("a", "b")),
    makeDiscreteParam("x", default = "a", values = c("a", "b"))
  )
  expect_identical(evaluateParamExpressions(ps, dict = list(p = 3, z = "b")), ps)
  expect_identical(evaluateParamExpressions(ps), ps)
  expect_identical(evaluateParamExpressions(ps, dict = list(p = 3)), ps)
  expect_identical(evaluateParamExpressions(ps, dict = list(z = "b")), ps)
})


test_that("expressions", {
  ps = makeParamSet(
    makeNumericParam("u", lower = expression(p)),
    makeIntegerParam("v", lower = 1, upper = expression(3 * p)),
    makeDiscreteParam("w", default = expression(z), values = c("a", "b")),
    makeDiscreteParam("x", default = "a", values = c("a", "b")),
    keys = c("p", "z")
  )
  ps2 = makeParamSet(
    makeNumericParam("u", lower = 3),
    makeIntegerParam("v", lower = 1, upper = 9),
    makeDiscreteParam("w", default = "b", values = c("a", "b")),
    makeDiscreteParam("x", default = "a", values = c("a", "b"))
  )
  dict = list(p = 3, z = "b")
  expect_identical(evaluateParamExpressions(ps, dict = dict), ps2)
  expect_error(evaluateParamExpressions(ps))
  expect_error(evaluateParamExpressions(ps, dict = list(p = 3)))
  expect_error(evaluateParamExpressions(ps, dict = list(z = "b")))
  pv = list(x = expression(k), y = 5)
  expect_identical(evaluateParamExpressions(pv, dict = list(k = 3)), list(x = 3, y = 5))
})
