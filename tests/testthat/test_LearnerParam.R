test_that("num vec", {
  p = makeNumericVectorLearnerParam("x", len = 2, lower = 0, upper = 2)
  expect_equal(p$id, "x")
  expect_equal(p$lower, c(0, 0))
  expect_equal(p$upper, c(2, 2))
  expect_equal(p$when, "train")
  expect_true(!isFeasible(p, 1))
  expect_true(isFeasible(p, c(1, 1)))
  p = makeNumericVectorLearnerParam("x", lower = 0, upper = 2, special.vals = list(NA_integer_))
  expect_equal(p$lower, 0)
  expect_equal(p$upper, 2)
  expect_true(isFeasible(p, 1))
  expect_true(isFeasible(p, c(1, 1)))
  expect_true(isFeasible(p, NA_integer_))
  # defaults
  p = makeNumericVectorLearnerParam(id = "x", allow.inf = TRUE, default = Inf)
  expect_error(makeNumericVectorLearnerParam(id = "x", allow.inf = FALSE, default = Inf), "feasible")
  expect_error(makeNumericVectorLearnerParam(id = "x", allow.inf = FALSE, default = c(0, 0), lower = c(-1, -1)), "'lower' must be either 1 or length of param")
  expect_error(makeNumericVectorLearnerParam(id = "x", allow.inf = FALSE, default = c(0, 0), upper = c(1, 1)), "'upper' must be either 1 or length of param")
})

test_that("int vec", {
  p = makeIntegerVectorLearnerParam("x", len = 2, lower = 0L, upper = 2L)
  expect_equal(p$id, "x")
  expect_equal(p$lower, c(0, 0))
  expect_equal(p$upper, c(2, 2))
  expect_equal(p$when, "train")
  expect_true(!isFeasible(p, 1))
  expect_true(isFeasible(p, c(1, 1)))
  p = makeIntegerVectorLearnerParam("x", lower = 0L, upper = 2L)
  expect_equal(p$lower, 0)
  expect_equal(p$upper, 2)
  expect_true(isFeasible(p, 1))
  expect_true(isFeasible(p, c(1, 1)))
  # defaults
  p = makeIntegerVectorLearnerParam(id = "x", len = NA_integer_, default = 1L)
  p = makeIntegerVectorLearnerParam(id = "x", len = NA_integer_, default = c(1L, 2L))
})

test_that("log vec", {
  p = makeLogicalVectorLearnerParam("x", len = 2)
  expect_equal(p$id, "x")
  expect_equal(p$values, list("TRUE" = TRUE, "FALSE" = FALSE))
  expect_true(!isFeasible(p, TRUE))
  expect_true(isFeasible(p, c(TRUE, FALSE)))
  p = makeLogicalVectorLearnerParam("x")
  expect_true(isFeasible(p, c(FALSE)))
  expect_true(isFeasible(p, c(TRUE, FALSE)))
  # defaults
  p = makeLogicalVectorLearnerParam(id = "x", len = NA_integer_, default = TRUE)
  p = makeLogicalVectorLearnerParam(id = "x", len = NA_integer_, default = c(TRUE, FALSE))
})

test_that("disc vec", {
  m = matrix(0, 2, 2)
  p = makeDiscreteVectorLearnerParam("x", len = 2, values = list(a = "a", b = m))
  expect_equal(p$id, "x")
  expect_equal(p$values, list(a = "a", b = m))
  expect_true(!isFeasible(p, "a"))
  expect_true(isFeasible(p, list("a", "a")))
  expect_true(isFeasible(p, list(m, "a")))
  p = makeDiscreteVectorLearnerParam("x", values = list(a = "a", b = m))
  expect_true(isFeasible(p, list("a")))
  expect_true(isFeasible(p, list(m)))
  expect_true(isFeasible(p, list("a", m, m)))
  # defaults
  p = makeDiscreteVectorLearnerParam(id = "x", len = NA_integer_, values = list("a", "b"), default = list("a"))
  p = makeDiscreteVectorLearnerParam(id = "x", len = NA_integer_, values = list("a", "b"), default = list("a", "b", "a"))
})


test_that("untyped", {
  p = makeUntypedLearnerParam("x")
  expect_equal(p$id, "x")
  expect_true(isFeasible(p, 1))
  expect_true(isFeasible(p, c()))
  expect_true(isFeasible(p, NULL))
  p = makeUntypedLearnerParam("x", default = NULL)
})


test_that("s4 objs works for values", {
  setClass("Dummy", representation(name = "character", id = "numeric"))
  obj.a = new("Dummy", name = "foo", id = 1)
  obj.b = new("Dummy", name = "bar", id = 2)
  vals = list(a = obj.a, b = obj.b)
  p = makeDiscreteLearnerParam("x", values = vals)
  expect_true(isFeasible(p, new("Dummy", name = "foo", id = 1)))
  expect_false(isFeasible(p, new("Dummy", name = "foo", id = 2)))
  p = makeDiscreteLearnerParam("x", values = vals, default = new("Dummy", name = "foo", id = 1))
  expect_output(print(p), "a,b")
})

test_that("unknown length works", {
  p = makeNumericVectorLearnerParam("x", len = NA, lower = 1)
  expect_true(isFeasible(p, c(2)))
  expect_true(isFeasible(p, c(2, 3)))
  expect_false(isFeasible(p, c(0)))
  expect_false(isFeasible(p, c(0, 0)))
  expect_error(sampleValue(p), "Cannot sample")
})

test_that("expressions work", {
  ps = makeParamSet(
    makeNumericVectorLearnerParam("x"),
    makeNumericVectorLearnerParam("y", upper = expression(n)),
    keys = "n"
  )
  expect_equal(getLower(ps), c(x = -Inf, y = -Inf))
  expect_equal(getUpper(ps, dict = list(n = 100)), c(x = Inf, y = 100))
  x = evaluateParamExpressions(ps, dict = list(n = 101))
  expect_equal(x$pars$x$upper, Inf)
  expect_equal(x$pars$y$upper, 101)
})
