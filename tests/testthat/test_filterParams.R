test_that("filter empty paramset", {
  ps = makeParamSet()
  expect_true(isEmpty(filterParams(ps, type = "numeric")))
  expect_true(isEmpty(filterParams(ps, type = "integer")))
  expect_true(isEmpty(filterParams(ps, type = "numericvector")))
  expect_true(isEmpty(filterParams(ps, type = "integervector")))
  expect_true(isEmpty(filterParams(ps, type = "discrete")))
  expect_true(isEmpty(filterParams(ps, type = "discretevector")))
  expect_true(isEmpty(filterParams(ps, type = "logical")))
  expect_true(isEmpty(filterParams(ps, type = "logicalvector")))
  expect_true(isEmpty(filterParams(ps, type = "character")))
  expect_true(isEmpty(filterParams(ps, type = "character")))
  expect_true(isEmpty(filterParams(ps, type = "function")))
  expect_true(isEmpty(filterParams(ps, type = "untyped")))
})

test_that("filter mixed paramset", {
  ps = makeParamSet(
    makeNumericParam("u", lower = 1),
    makeIntegerParam("v", lower = 1, upper = 2),
    makeDiscreteParam("w", values = 1:2),
    makeLogicalParam("x"),
    makeDiscreteVectorParam("y", len = 2, values = c("a", "b")),
    makeLogicalVectorParam("z", len = 3),
    makeCharacterVectorParam("s", len = 2)
  )
  expect_equal(getParamIds(filterParams(ps, type = "numeric")), "u")
  expect_equal(getParamIds(filterParams(ps, type = "integer")), "v")
  expect_equal(getParamIds(filterParams(ps, type = "discrete")), "w")
  expect_equal(getParamIds(filterParams(ps, type = "logical")), "x")
  expect_equal(getParamIds(filterParams(ps, type = c("logical", "logicalvector"))), c("x", "z"))
  expect_equal(getParamIds(filterParams(ps, type = c("character", "charactervector"))), "s")
  expect_equal(getParamIds(filterParams(ps, type = "discretevector")), "y")
  expect_equal(getParamIds(filterParams(ps, type = c("numeric", "integer"))), c("u", "v"))
  expect_equal(getParamIds(filterParams(ps, type = c("integer", "numeric"))), c("u", "v"))
  expect_equal(getParamIds(filterParams(ps, type = c("integer", "function"))), "v")
  expect_true(isEmpty(filterParams(ps, type = "function")))
})

test_that("mix filtering of type and tunable", {
  ps = makeParamSet(
    makeNumericParam("u", lower = 1),
    makeNumericParam("v", lower = 1, tunable = FALSE),
    makeDiscreteParam("w", values = 1:2),
    makeIntegerParam("x", lower = 1, upper = 2),
    makeLogicalVectorParam("y", len = 3),
    makeUntypedParam("z")
  )
  expect_equal(getParamIds(filterParams(ps, type = "numeric")), c("u", "v"))
  expect_equal(getParamIds(filterParams(ps, type = NULL)), c("u", "v", "w", "x", "y", "z"))
  expect_equal(getParamIds(filterParams(ps, type = c("numeric", "integer"), tunable = TRUE)), c("u", "x"))
  expect_equal(getParamIds(filterParams(ps, type = NULL, tunable = FALSE)), c("v"))
  expect_error(getParamIds(filterParams(ps, type = NULL, tunable = c(FALSE, FALSE))))
  expect_error(getParamIds(filterParams(ps, type = NULL, tunable = NULL)))
})

test_that("filtering of ids", {
  ps = makeParamSet(
    makeNumericParam("u", lower = 1),
    makeNumericParam("v", lower = 1, tunable = FALSE),
    makeDiscreteParam("w", values = 1:2),
    makeIntegerParam("x", lower = 1, upper = 2),
    makeLogicalVectorParam("y", len = 3),
    makeCharacterParam("s"),
    makeUntypedParam("z")
  )
  expect_error(filterParams(ps, type = "numeric", ids = c("a", "u")))
  expect_equal(getParamIds(filterParams(ps, type = "numeric", ids = c("v", "w", "y"))), "v")
  expect_equal(getParamIds(filterParams(ps, type = "character", ids = c("u", "v", "w", "s"))), "s")
  expect_equal(getParamIds(filterParams(ps, type = NULL, ids = c("v", "w", "y"))), c("v", "w", "y"))
  expect_equal(getParamIds(filterParams(ps, type = c("numeric", "integer"), tunable = TRUE, ids = c("w", "x", "y"))), "x")
  expect_true(isEmpty(filterParams(ps, type = "logical", ids = c("u", "v"))))
})

test_that("filtering with requirements", {
  ps = makeParamSet(
    makeDiscreteParam("x", values = c("a", "b"), default = "a"),
    makeNumericParam("y", requires = quote(x == "a"))
  )
  expect_error(filterParams(par.set = ps, type = "numeric", check.requires = TRUE), "Params x filtered")
})
