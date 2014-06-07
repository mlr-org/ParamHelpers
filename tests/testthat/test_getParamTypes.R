context("getParamTypes")

test_that("getParamTypes", {
  ps = makeParamSet(
    makeDiscreteParam("x", values = c("a", "b")),
    makeNumericVectorParam("y", len=2),
    makeIntegerParam("z",)
  )

  expect_equal(
    getParamTypes(ps, df.cols=FALSE, use.names=FALSE),
    c("discrete", "numericvector", "integer")
  )
  expect_equal(
    getParamTypes(ps, df.cols=TRUE, use.names=FALSE),
    c("factor", "numeric", "numeric", "integer")
  )
  expect_equal(
    getParamTypes(ps, df.cols=FALSE, use.names=TRUE),
    c(x="discrete", y="numericvector", z="integer")
  )
  expect_equal(
    getParamTypes(ps, df.cols=TRUE, use.names=TRUE, with.nr=FALSE),
    c(x="factor", y="numeric", y="numeric", z="integer")
  )
  expect_equal(
    getParamTypes(ps, df.cols=TRUE, use.names=TRUE, with.nr=TRUE),
    c(x="factor", y1="numeric", y2="numeric", z="integer")
  )
})

