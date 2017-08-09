context("generateDesignOfDefaults")


test_that("generateDesignOfDefaults", {
  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 5, default = 1),
    makeIntegerParam("y", lower = 2, upper= 6, default = 3)
  )
  d = generateDesignOfDefaults(ps)
  e = data.frame(x = 1, y = 3)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  ps = makeParamSet(
    makeNumericParam("u", lower = 1, upper = 5, default = 1),
    makeIntegerParam("v", lower = 2, upper= 6, default = 3),
    makeLogicalParam("w", default = TRUE),
    makeDiscreteParam("x", values = c("a", "b"), default = "a")
  )
  d = generateDesignOfDefaults(ps)
  e = data.frame(u = 1, v = 3, w = TRUE, x = factor("a", levels = c("a", "b")))
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  # vectors
  ps = makeParamSet(
    makeNumericVectorParam("x", len = 2L, lower = 1, upper = 2, default = c(1, 2)),
    makeIntegerVectorParam("y", len = 2L, lower = 3, upper = 4, default = c(3,3)),
    makeLogicalVectorParam("z", len = 2L, default = c(TRUE, FALSE))
  )
  d = generateDesignOfDefaults(ps)
  e = data.frame(x1 = 1, x2 = 2, y1 = 3, y2 = 3, z1 = TRUE, z2 = FALSE)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  # trafo
  ps = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 1, default = 0),
    makeNumericParam("y", lower = 3, upper = 4, trafo = function(x) 2*x, default = 3)
  )
  d = generateDesignOfDefaults(ps, trafo = TRUE)
  e = data.frame(x = 0, y = 6)
  attr(e, "trafo") = TRUE
  expect_equal(d, e)

  # missing default
  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 5),
    makeIntegerParam("y", lower = 2, upper= 6, default = 3)
  )
  expect_error(generateDesignOfDefaults(ps), regexp = "No default parameter setting for: x")

  # dependent parameter spaces
  ps = makeParamSet(
    makeNumericParam("x", lower = 1, upper = 3, default = 2),
    makeNumericParam("y", lower = 1, upper = 3, default = 1, requires = quote(x > 2))
  )
  d = generateDesignOfDefaults(ps)
  e = data.frame(x = 2, y = NA_real_)
  attr(e, "trafo") = FALSE
  expect_equal(d, e)

  # correct type
  ps = makeParamSet(
    makeIntegerParam("x", lower = 1L, upper = 3L, default = 2)
  )
  d = generateDesignOfDefaults(ps)
  e = data.frame(x = 2L)
  attr(e, "trafo") = FALSE
  expect_identical(class(d[,1]), class(e[,1]))

  # special vals
  ps = makeParamSet(
    makeNumericParam(id = "x1", lower = 0, upper = 2, default = "BLA", special.vals = list("BLA")),
    makeNumericParam(id = "x2", lower = 0, upper = 2, default = iris, special.vals = list(iris))
  )
  expect_error(generateDesignOfDefaults(ps), regexp = "special.vals as default for Parameter(s): x1,x2", fixed = TRUE)
})



test_that("generateDesignOfDefaults works with discrete params and complex values", {

  ps = makeParamSet(
    makeDiscreteParam("p", values = c("a", "b"), default = "b")
  )
  d = generateDesignOfDefaults(ps)
  expect_equal(d, data.frame(p = factor("b", levels = c("a", "b"))), check.attributes = FALSE)

  ps = makeParamSet(
    makeDiscreteParam("p", values = c(ir = "ir", foo = "bar"), default = "ir")
  )
  d = generateDesignOfDefaults(ps)
  expect_equal(d, data.frame(p = factor("ir", levels = c("ir", "foo"))), check.attributes = FALSE)

  p = makeDiscreteParam("p", values = c(ir = "ir", foo = "bar"), default = "bar")
  ps = makeParamSet(p)
  d = generateDesignOfDefaults(ps)
  expect_equal(d, data.frame(p = factor("foo", levels = c("ir", "foo"))), check.attributes = FALSE)

  p = makeDiscreteParam("p", values = list(ir = "ir", foo = iris), default = iris)
  ps = makeParamSet(p)
  d = generateDesignOfDefaults(ps)
  expect_equal(d, data.frame(p = factor("foo", levels = c("ir", "foo"))), check.attributes = FALSE)
})

