test_that("hasFiniteBoxConstraints", {
  par = makeParamSet()
  expect_true(hasFiniteBoxConstraints(par))

  par = makeNumericParam("x")
  expect_false(hasFiniteBoxConstraints(par))

  par = makeNumericParam("x", lower = -10, upper = 10)
  expect_true(hasFiniteBoxConstraints(par))

  par = makeNumericParam("x", lower = 0, upper = expression(n))
  expect_true(hasFiniteBoxConstraints(par, dict = list(n = 10)))
  expect_false(hasFiniteBoxConstraints(par, dict = list(n = Inf)))

  par.set = makeParamSet(
    makeNumericParam("numeric1", lower = -100, upper = 100),
    makeIntegerParam("integer1", lower = 0L, upper = 15L)
  )

  par.set = makeParamSet(
    makeNumericParam("numeric1", lower = -100, upper = 100),
    makeIntegerParam("integer1", lower = 0L, upper = 15L),
    makeDiscreteParam("discrete1", values = letters[1:2]),
    makeIntegerParam("integer2")
  )

  expect_false(hasFiniteBoxConstraints(par.set))

  par.set = makeParamSet(
    makeNumericParam("numeric1", lower = 1, upper = expression(n))
  )
  expect_true(hasFiniteBoxConstraints(par.set, dict = list(n = 10)))
  expect_false(hasFiniteBoxConstraints(par.set, dict = list(n = Inf)))
})
