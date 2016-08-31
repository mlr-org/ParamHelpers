context("generateDefaultDesign")


test_that("generateDefaultDesign", {
  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 5, default = 1),
    makeIntegerParam("y", lower = 2, upper= 6, default = 3)
  )
  d = generateDefaultDesign(ps)
  e = data.frame(x = 1, y = 3)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)
  
  ps = makeParamSet(
    makeNumericParam("u", lower = 1, upper = 5, default = 1),
    makeIntegerParam("v", lower = 2, upper= 6, default = 3),
    makeLogicalParam("w", default = TRUE),
    makeDiscreteParam("x", values = c("a", "b"), default = "a")
  )
  d = generateDefaultDesign(ps)
  e = data.frame(u = 1, v = 3, w = TRUE, x = factor("a", levels = c("a", "b")))
  attr(e, "trafo") = FALSE
  expect_equal(d, e)
  
  # vectors
  ps = makeParamSet(
    makeNumericVectorParam("x", len = 2L, lower = 1, upper = 2, default = c(1, 2)),
    makeIntegerVectorParam("y", len = 2L, lower = 3, upper = 4, default = c(3,3)),
    makeLogicalVectorParam("z", len = 2L, default = c(TRUE, FALSE))
  )
  d = generateDefaultDesign(ps)
  e = data.frame(x1 = 1, x2 = 2, y1 = 3, y2 = 3, z1 = TRUE, z2 = FALSE)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)
  
  # trafo
  ps = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 1, default = 0),
    makeNumericParam("y", lower = 3, upper = 4, trafo = function(x) 2*x, default = 3)
  )
  d = generateDefaultDesign(ps, trafo = TRUE)
  e = data.frame(x = 0, y = 6)
  attr(e, "trafo") = TRUE
  expect_equal(d, e)
  
  # missing default
  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 5),
    makeIntegerParam("y", lower = 2, upper= 6, default = 3)
  )
  expect_error(generateDefaultDesign(ps), regexp = "*No default parameter setting for x")
})