context("isSpecialValue")

test_that("isSpecialValue", {
  p = makeIntegerParam("a", special.vals = list(NULL))
  expect_true(isSpecialValue(p, NULL))
  expect_false(isSpecialValue(p, 1))
  expect_false(isSpecialValue(p, 1.5))
  expect_false(isSpecialValue(p, iris))
  p = makeIntegerParam("a", special.vals = list(iris, 1.5))
  expect_false(isSpecialValue(p, NULL))
  expect_false(isSpecialValue(p, 1))
  expect_true(isSpecialValue(p, 1.5))
  expect_true(isSpecialValue(p, iris))
})
