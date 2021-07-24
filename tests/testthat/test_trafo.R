test_that("trafoValue with param", {
  p = makeNumericParam(id = "x", lower = -10, upper = 10, trafo = function(x) x^2)
  expect_equal(trafoValue(p, 1), 1)
  expect_equal(trafoValue(p, -5), 25)
})

test_that("trafoValue with param set", {
  ps = makeParamSet(
    makeIntegerParam("u", trafo = function(x) 2 * x),
    makeNumericVectorParam("v", len = 2, trafo = function(x) x / sum(x)),
    makeDiscreteParam("w", values = c("a", "b"), trafo = function(x) paste0("=", x, "=")),
    makeDiscreteVectorParam("x", values = letters, len = 3, trafo = sort)
  )
  expect_equal(trafoValue(ps, list(3, c(2, 4), "a", c("b", "a", "c"))), list(u = 6, v = c(2 / 6, 4 / 6), w = "=a=", x = c("a", "b", "c")))
  # check if error is thrown when list has different names
  expect_error(trafoValue(ps, list(a = 1, b = 1, c = "b", d = c("a", "b", "c"))))
  # check if trafo function is applied on correct list-slots
  expect_equal(trafoValue(ps, list(w = "b", v = 1:2, u = 1, x = c("x", "f", "p"))), list(u = 2 * 1, v = 1:2 / sum(1:2), w = "=b=", x = c("f", "p", "x")))
})


test_that("trafo opt.path", {
  ps = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 2, trafo = function(x) 2^x)
  )
  op = makeOptPathDF(par.set = ps, y.names = "y", minimize = TRUE, add.transformed.x = FALSE)
  addOptPathEl(op, x = list(x = -2), y = 0)
  addOptPathEl(op, x = list(x = 2), y = 0)
  expect_error(addOptPathEl(op, x = list(x = 3), y = 0), "infeasible")
  op2 = trafoOptPath(op)
  df = as.data.frame(op2)
  expect_equal(df$x, c(1 / 4, 4))
  expect_error(trafoOptPath(op2), "Cannot further trafo")

  ps = makeParamSet(
    makeIntegerParam("u", trafo = function(x) 2 * x),
    makeNumericVectorParam("v", len = 2, trafo = function(x) x / sum(x)),
    makeDiscreteParam("w", values = c("a", "b"))
  )
  op = makeOptPathDF(ps, "y", TRUE)
  addOptPathEl(op, x = list(3, c(2, 4), "a"), y = 0, dob = 1, eol = 1)
  addOptPathEl(op, x = list(4, c(5, 3), "b"), y = 2, dob = 5, eol = 7)
  op2 = trafoOptPath(op)
  df2 = as.data.frame(op2)
  df2b = rbind(
    data.frame(u = 6, v1 = 2 / 6, v2 = 4 / 6, w = "a", y = 0, dob = 1, eol = 1,
      stringsAsFactors = TRUE),
    data.frame(u = 8, v1 = 5 / 8, v2 = 3 / 8, w = "b", y = 2, dob = 5, eol = 7,
      stringsAsFactors = TRUE)
  )
  expect_equal(df2, df2b)
})

test_that("trafo opt.path does not drop errmsg, exectime or extras", {
  ps = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 2, trafo = function(x) 2^x)
  )
  op = makeOptPathDF(par.set = ps, y.names = "y", minimize = TRUE, add.transformed.x = FALSE,
    include.error.message = TRUE, include.exec.time = TRUE, include.extra = TRUE)
  addOptPathEl(op, x = list(x = -2), y = 0,
    error.message = "foo", exec.time = 10, extra = list(e = 33, .f = iris))
  op2 = trafoOptPath(op)
  df = as.data.frame(op2, include.rest = TRUE)
  expect_equal(df, data.frame(x = 1 / 4, y = 0, dob = 1, eol = NA_real_,
    error.message = "foo", exec.time = 10, e = 33, stringsAsFactors = FALSE))
  el = getOptPathEl(op2, 1L)
  expect_equal(el$extra$.f, iris)
})
