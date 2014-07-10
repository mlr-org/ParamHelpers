context("cnames")

test_that("cnames work with different operations", {
  p = makeNumericVectorParam(id = "x", len = 2L, lower = 0, upper = 1, cnames = c("a", "b"))

  x = sampleValue(p)
  expect_equal(names(x), c("a", "b"))
  xs = sampleValues(p, 2L)
  lapply(xs, function(x) expect_equal(names(x), c("a", "b")))

  expect_true(isFeasible(p, x))
  expect_false(isFeasible(p, c(0.1, 0.1)))

  ps = makeParamSet(p)
  des = generateDesign(2, ps)
  xs = dfRowsToList(des, ps)
  lapply(xs, function(x) expect_equal(names(x$x), c("a", "b")))

  p = makeIntegerVectorParam(id = "x", len = 2L, lower = 0, upper = 10, cnames = c("a", "b"))
  x = setValueCNames(p, c(1L, 1L))
  expect_equal(x, c(a = 1L, b = 1L))
  p = makeIntegerParam(id = "x")
  x = setValueCNames(p, 1L)
  expect_equal(x, 1L)

})

