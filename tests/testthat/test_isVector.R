test_that("isVector", {
  ps = makeParamSet()
  expect_true(isVector(ps))

  non.vector.ps = makeParamSet(
    makeDiscreteParam("disc", values = letters[1:3]),
    makeLogicalVectorParam("logi", len = 2L)
  )

  expect_false(isVector(non.vector.ps))
  expect_false(isVector(non.vector.ps$pars[[1L]]))
  expect_true(isVector(non.vector.ps$pars[[2L]]))

  vector.ps = makeParamSet(
    makeNumericVectorParam("num", len = 2L, lower = 0, upper = 5),
    makeDiscreteVectorParam("disc", len = 4L, values = letters[1:3])
  )

  expect_true(isVector(vector.ps))
  expect_true(isVector(vector.ps$pars[[1L]]))
  expect_true(isVector(vector.ps$pars[[2L]]))
})
