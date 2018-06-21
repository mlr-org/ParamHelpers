context("splitVectorParams")

test_that("splitVectorParams", {
  p1 = makeNumericParam("p1", lower = 1)
  expect_equal(splitVectorParams(p1), list(p1 = p1))

  p2 = makeNumericVectorParam(id = "p2", lower = -1, upper = 1, len = 2)
  expect_equal(splitVectorParams(p2), list(
    p21 = makeNumericParam("p21", lower = -1, upper = 1),
    p22 = makeNumericParam("p22", lower = -1, upper = 1)
  ))

  p3 = makeDiscreteVectorParam("p3", len = 3, values = c("a", "b"))
  expect_equal(splitVectorParams(p3), list(
    p31 = makeDiscreteParam("p31", values = c("a", "b")),
    p32 = makeDiscreteParam("p32", values = c("a", "b")),
    p33 = makeDiscreteParam("p33", values = c("a", "b"))
  ))

  ps = makeParamSet(p1)
  expect_equal(splitVectorParams(ps), makeParamSet(p1))

  ps = makeParamSet(p1, p2, p3)
  ps2 = splitVectorParams(ps)
  expect_equal(ps2, makeParamSet(p1,
    p21 = makeNumericParam("p21", lower = -1, upper = 1),
    p22 = makeNumericParam("p22", lower = -1, upper = 1),
    p31 = makeDiscreteParam("p31", values = c("a", "b")),
    p32 = makeDiscreteParam("p32", values = c("a", "b")),
    p33 = makeDiscreteParam("p33", values = c("a", "b"))
  ))
  expect_true(isFeasible(ps2, list(p1 = 1, p21 = 0, p22 = 0, p31 = "a", p32 = "a", p33 = "a")))
})


