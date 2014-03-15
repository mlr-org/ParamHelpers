context("isPurely{Discrete, Integer, Numeric} and has{Discrete, Integer, Numeric}")

test_that("isPurely{Discrete, Integer, Numeric} and has{Discrete, Integer, Numeric}", {
  par.set.empty = makeParamSet()
  methods = c(isPurelyInteger, isPurelyDiscrete, isPurelyNumeric,
    hasInteger, hasDiscrete, hasNumeric)

  lapply(methods, function(fun) {
    expect_false(fun(par.set.empty))
  })

  par.set.mixed = makeParamSet(
    makeNumericParam("u", lower=1),
    makeIntegerParam("v", lower=1, upper=2),
    makeDiscreteParam("w", values=1:2),
    makeLogicalParam("x"),
    makeDiscreteVectorParam("y", len=2, values=c("a", "b"))
  )

  par.set.discrete = makeParamSet(
    makeDiscreteParam("discr1", values = 1:2),
    makeDiscreteParam("discr2", values = letters[1:3]),
    makeDiscreteVectorParam("y", len=2, values=c("x", "y"))
  )

  expect_true(isPurelyDiscrete(par.set.discrete))
  expect_false(isPurelyInteger(par.set.discrete))
  expect_false(isPurelyNumeric(par.set.discrete))
  expect_false(hasNumeric(par.set.discrete))
  expect_false(hasInteger(par.set.discrete))

  expect_false(isPurelyDiscrete(par.set.mixed))
  expect_false(isPurelyInteger(par.set.mixed))
  expect_false(isPurelyNumeric(par.set.mixed))
  expect_true(hasNumeric(par.set.mixed))
  expect_true(hasInteger(par.set.mixed))
})
