context("evaluateParamSet")

test_that("no changes", {
  ps = makeParamSet()
  expect_identical(evaluateParamSet(ps), ps)
  expect_identical(evaluateParamSet(ps, dict = list(p = 3)), ps)

  ps = makeParamSet(
    makeNumericParam("u", lower = 3),
    makeIntegerParam("v", lower = 1, upper = 9),
    makeDiscreteParam("w", default = "b", values = c("a", "b")),
    makeDiscreteParam("x", default = "a", values = c("a", "b"))
  )
  expect_identical(evaluateParamSet(ps, dict = list(p = 3, z = "b")), ps)
  expect_identical(evaluateParamSet(ps), ps)
  expect_identical(evaluateParamSet(ps, dict = list(p = 3)), ps)
  expect_identical(evaluateParamSet(ps, dict = list(z = "b")), ps)
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
  ee = list(p = 3, z = "b")
  expect_identical(evaluateParamSet(ps, dict = ee), ps2)
  expect_error(evaluateParamSet(ps))
  expect_error(evaluateParamSet(ps, dict = list(p = 3)))
  expect_error(evaluateParamSet(ps, dict = list(z = "b")))
})
