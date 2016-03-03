context("checkParamSet")

test_that("easy use-cases", {
  ps = makeParamSet()
  expect_true(checkParamSet(ps))
  expect_true(checkParamSet(ps, envir = list(p = 3)))
  
  ps = makeParamSet(
    makeNumericParam("u", lower = 3),
    makeIntegerParam("v", lower = 1, upper = 9),
    makeDiscreteParam("w", default = "b", values = c("a", "b")),
    makeDiscreteParam("x", default = "a", values = c("a", "b"))
  )
  expect_true(checkParamSet(ps, envir = list(p = 3, z = "b")))
  expect_true(checkParamSet(ps))
  expect_true(checkParamSet(ps, envir = list(p = 3)))
  expect_true(checkParamSet(ps, envir = list(z = "b")))
})


test_that("expressions", {
  ps = makeParamSet(
    makeNumericParam("u", lower = expression(p)),
    makeIntegerParam("v", lower = 1, upper = expression(3 * p)),
    makeDiscreteParam("w", default = expression(z), values = c("a", "b")),
    makeDiscreteParam("x", default = "a", values = c("a", "b")),
    dictionary = c("p", "z")
  )
  expect_true(checkParamSet(ps, envir = list(p = 3, z = "b")))
  expect_error(checkParamSet(ps))
  expect_error(checkParamSet(ps, envir = list(p = 3)))
  expect_error(checkParamSet(ps, envir = list(z = "b")))
})
