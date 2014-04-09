context("generateGridDesign")


test_that("simple num/int/bool grid design", {
  ps2 = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1),
    makeIntegerParam("x2", lower=10, upper=20, trafo=sqrt),
    makeLogicalParam("x3")
  )
  des = generateGridDesign(n = c(4, 2, 10), par.set=ps2)
  expect_equal(nrow(des), 16)
  expect_equal(ncol(des), 3)
  expect_true(is.numeric(des[,1]))
  expect_true(is.numeric(des[,2]))
  expect_true(is.logical(des[,3]))
  expect_true(is.integer(des[,2]))
  expect_equal(colnames(des), getParamIds(ps2))
  des = generateGridDesign(n = c(2,5,2), par.set=ps2, trafo=TRUE)
  expect_true(is.numeric(des[,2]))
  expect_true(length(setdiff(des[,2], round(sqrt(seq(from=10, to=20, length.out=5))))) == 0)
})