test_that("discretizeParam", {

  p = makeNumericParam("x", lower = -1, upper = 1)
  d1 = discretizeParam(p, by = 0.2)
  d2 = makeDiscreteParam("x", values = seq(-1L, 1L, by = 0.2))
  expect_equal(d1, d2)

  p = makeIntegerParam("x", lower = 0, upper = 10)
  d1 = discretizeParam(p, by = 2)
  d2 = makeDiscreteParam("x", values = seq(0, 10, by = 2))
  expect_equal(d1, d2)

  p = makeLogicalParam("x")
  expect_equal(discretizeParam(p), p)
  p = makeDiscreteParam("x", values = c("a", "b"))
  expect_equal(discretizeParam(p), p)

  p = makeUntypedParam("x")
  expect_error(discretizeParam(p), "untyped")
})


