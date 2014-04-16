context("generateGridDesign")

test_that("generateGridDesign", {
  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 5),
    makeIntegerParam("y", lower = 2, upper= 6)
  )
  d = generateGridDesign(ps, resolution = 3L, ints.as.num = FALSE)
  e = expand.grid(x = c(1, 3, 5), y = c(2L, 4L, 6L), KEEP.OUT.ATTRS = FALSE)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)
  d = generateGridDesign(ps, resolution = 3L, ints.as.num = TRUE)
  e = expand.grid(x = c(1, 3, 5), y = c(2, 4, 6), KEEP.OUT.ATTRS = FALSE)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 5),
    makeIntegerParam("y", lower = 2, upper= 6),
    makeLogicalParam("z")
  )
  d = generateGridDesign(ps, resolution = 3L, logicals.as.factor = FALSE)
  e = expand.grid(x = c(1, 3, 5), y = c(2L, 4L, 6L), z = c(TRUE, FALSE), KEEP.OUT.ATTRS = FALSE)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)
  d = generateGridDesign(ps, resolution = 3L, logicals.as.factor = TRUE)
  e = expand.grid(x = c(1, 3, 5), y = c(2L, 4L, 6L), z = c("TRUE", "FALSE"), KEEP.OUT.ATTRS = FALSE)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  # vectors
  ps = makeParamSet(
    makeNumericVectorParam("x", len = 2L, lower = 1, upper = 2),
    makeIntegerVectorParam("y", len = 2L, lower = 3, upper = 4),
    makeLogicalVectorParam("z", len = 2L)
  )
  d = generateGridDesign(ps, resolution = 2L)
  e = expand.grid(
    x1 = c(1, 2), x2 = c(1, 2),
    y1 = c(3L, 4L), y2 = c(3L, 4L),
    z1 = c(TRUE, FALSE), z2 = c(TRUE, FALSE),
    KEEP.OUT.ATTRS = FALSE
  )
  attr(e, "trafo") = FALSE
  expect_equal(d, e)
  ps = makeParamSet(
    makeNumericVectorParam("x", len = 2L, lower = 1, upper = 2),
    makeIntegerVectorParam("y", len = 2L, lower = 3, upper = 4),
    makeLogicalVectorParam("z", len = 2L)
  )
  d = generateGridDesign(ps, resolution = 2L, logicals.as.factor = TRUE, ints.as.num = TRUE)
  e = expand.grid(
    x1 = c(1, 2), x2 = c(1, 2),
    y1 = c(3, 4), y2 = c(3, 4),
    z1 = c("TRUE", "FALSE"), z2 = c("TRUE", "FALSE"),
    KEEP.OUT.ATTRS = FALSE
  )
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  # trafo
  ps = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 1),
    makeNumericParam("y", lower = 3, upper = 4, trafo = function(x) 2*x)
  )
  d = generateGridDesign(ps, resolution = c(x = 2, y = 4), trafo = TRUE)
  e = expand.grid(
    x = seq(0, 1, length.out = 2),
    y = sqrt(seq(3, 4, length.out = 4)),
    KEEP.OUT.ATTRS = FALSE
  )
  attr(e, "trafo") = TRUE
  expect_equal(d, e)
  ps = makeParamSet(
    makeNumericVectorParam("x", len = 2, lower = 0, upper = 1, trafo = function(x) x / sum(x))
  )
  d = generateGridDesign(ps, resolution = 4, trafo = TRUE)
  e = expand.grid(
    x1 = seq(0, 1, length.out = 4),
    x2 = seq(0, 1, length.out = 4),
    KEEP.OUT.ATTRS = FALSE
  )
  e = rowApply(e, function(x) x / sum(x))
  attr(e, "trafo") = TRUE
  expect_equal(d, e)
})
