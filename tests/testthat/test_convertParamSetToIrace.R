context("convertParamSetToIrace")

test_that("convertParamSetToIrace", {
  runIrace = function(ps, hook.run, max.exps = 10) {
    ip = convertParamSetToIrace(ps)
    expect_equal(getParamIds(ps, repeated = TRUE, with.nr = TRUE), as.character(ip$names))
    res = capture.output(
      irace(
        tunerConfig = list(
          hookRun = hook.run, 
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
  hook.run = function(instance, candidate, extra.params = NULL, config = list()) 1
  runIrace(ps, hook.run, max.exps = 300)
  ps = makeParamSet(
    makeDiscreteParam("x1", values = c("a", "b")),
    makeLogicalParam("x2", requires = quote(x1 == "a"))
  )
  ips = convertParamSetToIrace(ps)
  expect_false(identical(ips$constraints$x2, expression(TRUE)))
  hook.run = function(instance, candidate, extra.params = NULL, config = list()) {
    v = candidate$values
    if ((v$x1 == "a" && is.na(v$x2)) || (v$x1 == "b" && !is.na(v$x2)))
      stop("foo")
    1
  }
  runIrace(ps, hook.run, max.exps = 300)
})
