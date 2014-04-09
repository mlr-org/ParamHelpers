context("discretizeParam")

test_that("discretizeParam", {
  p = makeNumericParam(id="x", lower=-1, upper=1, trafo=sqrt)
  p.d = discretizeParam(p, by=0.2)
  p.d.m = makeDiscreteParam(id="x", values=seq(-1L, 1L, by=0.2), trafo=sqrt)
  expect_equal(class(p.d), class(p))
  expect_equal(p.d$type, "discrete")
  expect_equal(p.d$trafo, p$trafo)
  # FIXME: lower and upper have to be NULL but stay in the list.
  # expect_equal(p.d.m, p.d)
  expect_equal(p.d.m$values, p.d$values)
  expect_equal(discretizeParam(p.d.m), p.d.m)
  
  p = makeIntegerParam(id="x", lower=-5L, upper=5L)
  p.d = discretizeParam(p, length.out=11L)
  p.d.m = makeDiscreteParam(id="x", values=seq(-5L, 5L, length.out=11L))
  expect_equal(p.d.m$values, p.d$values)
  
  p = makeLogicalParam(id="l")
  expect_equal(p, discretizeParam(p))
})


test_that("setParamToDiscreteVals", {
  
})