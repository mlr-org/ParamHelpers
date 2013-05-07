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