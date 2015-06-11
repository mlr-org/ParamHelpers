context("FilterParams")

test_that("filter empty paramset", {
  ps = makeParamSet()
  expect_output(filterParams(ps, "numeric"), "Empty parameter set.")
  expect_output(filterParams(ps, "integer"), "Empty parameter set.")
  expect_output(filterParams(ps, "numericvector"), "Empty parameter set.")
  expect_output(filterParams(ps, "integervector"), "Empty parameter set.")
  expect_output(filterParams(ps, "discrete"), "Empty parameter set.")
  expect_output(filterParams(ps, "discretevector"), "Empty parameter set.")
  expect_output(filterParams(ps, "logical"), "Empty parameter set.")
  expect_output(filterParams(ps, "function"), "Empty parameter set.")
  expect_output(filterParams(ps, "untyped"), "Empty parameter set.")
})


test_that("filter mixed paramset", {
  ps = makeParamSet(
    makeNumericParam("u", lower=1),
    makeIntegerParam("v", lower=1, upper=2),
    makeDiscreteParam("w", values=1:2),
    makeLogicalParam("x"),
    makeDiscreteVectorParam("y", len=2, values=c("a", "b")),
    makeLogicalVectorParam("z", len=3)
  )
  expect_equal(getParamIds(filterParams(ps, "numeric")),"u")
  expect_equal(getParamIds(filterParams(ps, "integer")),"v")
  expect_equal(getParamIds(filterParams(ps, "discrete")),"w")
  expect_equal(getParamIds(filterParams(ps, "logical")),"x")
  expect_equal(getParamIds(filterParams(ps, c("logical", "logicalvector"))), c("x", "z"))
  expect_equal(getParamIds(filterParams(ps, "discretevector")),"y")
  expect_equal(getParamIds(filterParams(ps, c("numeric","integer"))),c("u","v"))
  expect_equal(getParamIds(filterParams(ps, c("integer","numeric"))),c("u","v"))
  expect_equal(getParamIds(filterParams(ps, c("integer","function"))),"v")
  expect_output(filterParams(ps, "function"), "Empty parameter set.")
})

test_that("mix filtering of type and tunable", {
  ps = makeParamSet(
    makeNumericParam("u", lower = 1),
    makeNumericParam("v", lower = 1, tunable = FALSE),
    makeDiscreteParam("w", values = 1:2),
    makeIntegerParam("x", lower = 1, upper = 2),
    makeLogicalVectorParam("y", len = 3),
    makeUntypedParam("z")
  )
  expect_equal(getParamIds(filterParams(ps, "numeric")), c("u", "v"))
  expect_equal(getParamIds(filterParams(ps, NULL)), c("u", "v", "w", "x", "y", "z"))
  expect_equal(getParamIds(filterParams(ps, c("numeric", "integer"), TRUE)), c("u", "x"))
  expect_equal(getParamIds(filterParams(ps, NULL, FALSE)), c("v", "z"))
  expect_error(getParamIds(filterParams(ps, NULL, c(FALSE, FALSE))))
  expect_error(getParamIds(filterParams(ps, NULL, NULL)))
})

test_that("filtering of ids", {
  ps = makeParamSet(
    makeNumericParam("u", lower = 1),
    makeNumericParam("v", lower = 1, tunable = FALSE),
    makeDiscreteParam("w", values = 1:2),
    makeIntegerParam("x", lower = 1, upper = 2),
    makeLogicalVectorParam("y", len = 3),
    makeUntypedParam("z")
  )
  expect_error(filterParams(ps, "numeric", ids = c("a", "u")))
  expect_equal(getParamIds(filterParams(ps, "numeric", ids = c("v", "w", "y"))), "v")
  expect_equal(getParamIds(filterParams(ps, NULL, ids = c("v", "w", "y"))), c("v", "w", "y"))
  expect_equal(getParamIds(filterParams(ps, c("numeric", "integer"), TRUE, c("w", "x", "y"))), "x")
  expect_output(filterParams(ps, "logical", ids = c("u", "v")), "Empty parameter set.")
})
