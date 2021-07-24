test_that("isFeasible ParamSet", {
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L),
    makeDiscreteParam("b", values = list("0" = "NIL", "1" = "ONE", "f" = function(x) x + 1)),
    makeIntegerParam("c", default = 1L, requires = quote(a == 1)),
    makeIntegerParam("d", default = 1L, requires = quote(a == 2)),
    makeIntegerParam("e", default = expression(n = 99L)),
    keys = "n"
  )
  expect_true(isFeasible(ps, list(a = 1, b = function(x) x + 1, c = 1, d = NA, e = 12L)))
  expect_true(isFeasible(ps, list(a = 1, b = function(x) x + 1, c = 1), filter = TRUE))
  expect_true(isFeasible(ps, list(a = 2, b = function(x) x + 1, d = 1), filter = TRUE))
  expect_error(isFeasible(ps, list(a = 1, c = 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1), filter = TRUE), "only works with named input")
  expect_error(isFeasible(ps, list(a = 1, zzz = 1), filter = TRUE), "zzz")
  expect_false((res = isFeasible(ps, list(a = 1, c = "1"), filter = TRUE)))
  expect_match(attr(res, "warning"), "c=1 does not meet constraints")
  expect_error(isFeasible(ps, list(c = 2), filter = TRUE), "needed for requirements: a")
  expect_false((res = isFeasible(ps, list(a = 2, c = 2), filter = TRUE)))
  expect_match(attr(res, "warning"), "Param c=2 is set but does not meet requirements")
  expect_true((res = isFeasible(ps, list(a = 2, c = NA), filter = TRUE)))
  expect_true(isFeasible(ps, list(c = 2), filter = TRUE, use.defaults = TRUE))

  # make sure we can ignore defaults if they contradict a new setting
  ps = makeParamSet(
    makeIntegerParam("a", default = 1L, requires = quote(b == 2)),
    makeIntegerParam("b", default = 2L, requires = quote(c == TRUE)),
    makeLogicalParam("c", default = TRUE))
  expect_error(isFeasible(ps, list(b = 3), filter = TRUE), "needed for requirements: c")
  expect_true(isFeasible(ps, list(b = 3), filter = TRUE, use.defaults = TRUE))
})

test_that("isFeasible LearnerParamSet", {
  ps = makeParamSet(
    makeIntegerLearnerParam("a", default = 1L),
    makeDiscreteLearnerParam("b", values = list("0" = "NIL", "1" = "ONE", "f" = function(x) x + 1)),
    makeIntegerLearnerParam("c", default = 1L, requires = quote(a == 1)),
    makeIntegerLearnerParam("d", default = 1L, requires = quote(a == 2))
  )

  expect_true(isFeasible(ps, list(a = 1, b = function(x) x + 1, c = 1, d = NA)))
  expect_true(isFeasible(ps, list(a = 1, b = function(x) x + 1, c = 1), filter = TRUE))
  expect_true(isFeasible(ps, list(a = 2, b = function(x) x + 1, d = 1), filter = TRUE))
  expect_error(isFeasible(ps, list(a = 1, c = 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1), filter = TRUE), "only works with named input")
  expect_error(isFeasible(ps, list(a = 1, zzz = 1), filter = TRUE), "zzz")
  expect_false((res = isFeasible(ps, list(a = 1, c = "1"), filter = TRUE)))
  expect_match(attr(res, "warning"), "c=1 does not meet constraints")
  expect_error(isFeasible(ps, list(c = 2), filter = TRUE), "needed for requirements: a")
  expect_false((res = isFeasible(ps, list(a = 2, c = 2), filter = TRUE)))
  expect_match(attr(res, "warning"), "Param c=2 is set but does not meet requirements")
  expect_true((res = isFeasible(ps, list(a = 2, c = NA), filter = TRUE)))
  expect_true(isFeasible(ps, list(c = 2), filter = TRUE, use.defaults = TRUE))

  ps = makeParamSet(
    makeIntegerLearnerParam("a", default = 1L),
    makeDiscreteLearnerParam("b", values = list("0" = "NIL", "1" = "ONE", "f" = function(x) x + 1)),
    makeIntegerLearnerParam("c", default = 1L, requires = quote(a == 1)),
    makeIntegerLearnerParam("d", default = 1L, requires = quote(a == 2)),
    makeIntegerLearnerParam("e", default = expression(n), upper = expression(2 * n)),
    keys = "n"
  )

  expect_true(isFeasible(ps, list(a = 1, b = function(x) x + 1, c = 1, d = NA, e = 12L)))
  expect_true(isFeasible(ps, list(a = 1, b = function(x) x + 1, c = 1), filter = TRUE))
  expect_true(isFeasible(ps, list(a = 2, b = function(x) x + 1, d = 1), filter = TRUE))
  expect_error(isFeasible(ps, list(a = 1, c = 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1)), "does not match ParamSet length")
  expect_error(isFeasible(ps, list(1, 1), filter = TRUE), "only works with named input")
  expect_error(isFeasible(ps, list(a = 1, zzz = 1), filter = TRUE), "zzz")
  expect_false((res = isFeasible(ps, list(a = 1, c = "1"), filter = TRUE)))
  expect_match(attr(res, "warning"), "c=1 does not meet constraints")
  expect_error(isFeasible(ps, list(c = 2), filter = TRUE), "needed for requirements: a")
  expect_false((res = isFeasible(ps, list(a = 2, c = 2), filter = TRUE)))
  expect_match(attr(res, "warning"), "Param c=2 is set but does not meet requirements")
  expect_true((res = isFeasible(ps, list(a = 2, c = NA), filter = TRUE)))
})

test_that("length of vectors", {
  ps = makeParamSet(
    makeIntegerVectorLearnerParam("a", default = 1L),
    makeNumericVectorLearnerParam("b", len = expression(n)),
    makeNumericVectorLearnerParam("c", len = 4L, lower = -1, upper = 5),
    keys = "n"
  )

  expect_true(isFeasible(ps, list(a = 1, b = 2, c = rep(3, 4))))
  expect_false((res = isFeasible(ps, list(a = 1, b = 2, c = 3))))
  expect_match(attr(res, "warning"), "The parameter setting c=3 does not meet constraints")
})

# we had a bug here, see issue #145
test_that("isFeasible works when len=NA, and default is given (with other lengths than in isFeasible)", {
  p = makeIntegerVectorLearnerParam(id = "test", default = c(10, 10), lower = 0)
  expect_true(isFeasible(p, c(10, 10, 10)))
  expect_false(isFeasible(p, c(-1)))
})

test_that("atomic requires", {
  ps = makeParamSet(
    makeLogicalParam("a"),
    makeIntegerParam("b", 0, 1, requires = quote(a))
  )

  expect_true(isFeasible(ps, list(a = TRUE, b = 0)))
  expect_true(isFeasible(ps, list(a = FALSE, b = NA)))
  expect_false(isFeasible(ps, list(a = TRUE, b = NA)))
  expect_false(isFeasible(ps, list(a = FALSE, b = 0)))
})
