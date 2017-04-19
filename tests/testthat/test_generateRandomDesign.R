context("generateRandomDesign")

test_that("num/int/disc vec design", {
  ps = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 1),
    makeIntegerVectorParam("y", len = 3, lower = 10L, upper = 20L),
    makeDiscreteVectorParam("z", len = 2, values = list(a = "a", b = list())),
    makeLogicalVectorParam("a", len = 2)
  )
  des = generateRandomDesign(13, ps)
  expect_equal(nrow(des), 13)
  expect_equal(ncol(des), 8)
  expect_equal(colnames(des), c("x", "y1", "y2", "y3", "z1", "z2", "a1", "a2"))
  expect_true(is.numeric(des[,1]))
  expect_true(is.integer(des[,2]))
  expect_true(is.integer(des[,3]))
  expect_true(is.integer(des[,4]))
  expect_true(is.factor(des[,5]))
  expect_true(is.factor(des[,6]))
  expect_true(is.logical(des[,7]))
  expect_true(is.logical(des[,8]))
  expect_true(des[,1] >= -2 && des[,1] <= 1)
  expect_true(des[,2] >= 10 && des[,2] <= 20)
  expect_true(des[,3] >= 10 && des[,3] <= 20)
  expect_true(des[,4] >= 10 && des[,4] <= 20)
  expect_true(all(des[,5] %in% c("a", "b")))
  expect_true(all(des[,6] %in% c("a", "b")))
  expect_equal(levels(des[,5]), c("a", "b"))
  expect_equal(levels(des[,6]), c("a", "b"))
})

test_that("requires works", {
  ps = makeParamSet(
    makeDiscreteParam("x", values = c("a", "b")),
    makeNumericParam("y", lower = 1, upper = 2, requires = quote(x == "a")),
    makeDiscreteParam("z", values = 1:2, requires = quote(x == "b"))
  )
  des = generateRandomDesign(50, par.set = ps)
  vals = dfRowsToList(des, ps)
  oks = sapply(vals, isFeasible, par = ps)
  expect_true(all(oks))
  ps = makeParamSet(
    makeDiscreteParam("x", values = c("a", "b")),
    makeNumericVectorParam("y", len = 2, lower = 1, upper = 2, requires = quote(x == "a"))
  )
  des = generateDesign(50, par.set = ps)
  vals = dfRowsToList(des, ps)
  oks = sapply(vals, isFeasible, par = ps)
  expect_true(all(oks))
})

test_that("we dont drop levels in factors", {
  ps = makeParamSet(
    makeDiscreteParam("x", values = letters[5:1])
  )
  des = generateRandomDesign(1, ps)
  expect_true(is.factor(des$x) && levels(des$x) == letters[5:1])
})

test_that("default param set values can be added", {
  #basic setup with one numeric and one discrete parameter
  ps = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 1, default = 0),
    makeDiscreteParam(id = "disc", values = letters[1:2], default = letters[1])
  )
  
  res = generateRandomDesign(10L, ps, add.default = TRUE)
  expect_equal(nrow(res), 10L)
  expect_equivalent(res[1, ], data.frame(x = 0, disc = factor(letters[1], levels = letters[1:2])))
  
  #setup with vector parameter
  ps = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 1, default = 0),
    makeNumericVectorParam("y", len = 2, lower = 0, upper = 1, default = c(1,0), trafo = function(x) x/sum(x))
  )
  
  res = generateRandomDesign(10L, ps, add.default = TRUE)
  expect_equal(nrow(res), 10)
  expect_equivalent(res[1, ], data.frame(x = 0, y1 = 1, y2 = 0))
})
