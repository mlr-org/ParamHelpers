context("convertParamSetToIrace")

test_that("convertParamSetToIrace", {
  requirePackages("_irace")
  runIrace = function(ps, target.runner, max.exps = 10) {
    ip = convertParamSetToIrace(ps)
    expect_equal(getParamIds(ps, repeated = TRUE, with.nr = TRUE), as.character(ip$names))
    res = capture.output(
      irace::irace(
        scenario = list(
          targetRunner = target.runner,
          instances = 1:10,
          maxExperiments = max.exps,
          logFile = tempfile()
        ),
        parameters = ip
      )
    )
  }

  ps = makeParamSet(
    makeLogicalParam("v1"),
    makeNumericParam("x1", lower = 1, upper = 4),
    makeIntegerParam("y1", lower = 1, upper = 4),
    makeDiscreteParam("z1", values = c("a", "b", "c")),
    makeLogicalVectorParam("v2", len = 2),
    makeNumericVectorParam("x2", len = 2, lower = 1:2, upper = pi),
    makeIntegerVectorParam("y2", len = 2, lower = 0:1, upper = 4),
    makeDiscreteVectorParam("z2", len = 2, values = c("a", "b", "c"))
  )
  target.runner = function(experiment, config = list()) list(cost = 1)
  runIrace(ps, target.runner, max.exps = 300)
  ps = makeParamSet(
    makeDiscreteParam("x1", values = c("a", "b")),
    makeLogicalParam("x2", requires = quote(x1 == "a")),
    makeLogicalParam("x3", requires =
      quote(x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE" && x1 == "a" && x2 == "FALSE"))
  )
  ips = convertParamSetToIrace(ps)
  expect_false(identical(ips$constraints$x2, expression(TRUE)))
  target.runner = function(experiment, config = list()) {
    v = experiment$configuration
    if ((v$x1 == "a" && is.na(v$x2)) || (v$x1 == "b" && !is.na(v$x2)))
      stop("foo")
    if ((v$x1 == "a" && v$x2 == "FALSE" && is.na(v$x3)) || (!(v$x1 == "a" && v$x2 == "FALSE") && !is.na(v$x3)))
      stop("requires failed")
    list(cost = 1)
  }
  runIrace(ps, target.runner, max.exps = 300)
})

test_that("convertParamSetToIrace checks box constraints", {
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "i", lower = 1L)
  )

  expect_error(convertParamSetToIrace(ps), "finite box")
})

test_that("convertParamSetToIrace uses correct boundaries", {
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("sigma", lower = 4e-9, upper = 2.123456724252662),
    makeIntegerParam("myInt", lower = 3, upper = 20),
    makeLogicalParam("Binar", default = TRUE)
  )
  ips = convertParamSetToIrace(ps)

  expect_identical(vcapply(ips$domain, function(x) class(x)),
    c(kernel = "character", sigma = "numeric", myInt = "numeric", Binar = "character"))
  expect_identical(ips$boundary$sigma, as.numeric(unlist(ps$pars$sigma[c("lower", "upper")])))
  expect_identical(ips$boundary$myInt, as.integer(unlist(ps$pars$myInt[c("lower", "upper")])))
})

test_that("convertParamSetToIrace work with vecparam", { # has issue here, see 85
  ps = makeParamSet(
    makeNumericVectorParam("a", len=2, -10, 10),
    makeDiscreteParam("b", c("v", "w")),
    makeNumericVectorParam("c", len=2, -10, 10)
  )
  i = convertParamSetToIrace(ps)
  b = i$domain
  expect_equal(b$a1, c(-10, 10))
  expect_equal(b$a2, c(-10, 10))
  expect_equal(b$b, c("v", "w"))
  expect_equal(b$c1, c(-10, 10))
  expect_equal(b$c2, c(-10, 10))
})
