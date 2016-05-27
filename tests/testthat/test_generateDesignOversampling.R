context("generateDesignOversampling")

# Since we mainly call generateRandomDesign, which is well tested,
# only a single test
test_that("generateDesignOversampling", {
  
  par.set = makeParamSet(
    makeDiscreteParam("x1", values = c("a", "b", "c")),
    
    makeIntegerParam("x2",
      lower = 15, upper = 45,
      requires = quote(x1 == "a")),
    makeIntegerParam("x3", lower = 1L, upper = 120,
      requires = quote(x1 == "a")),
    
    makeNumericParam("x4", lower = -15L, upper = 15L, trafo = function(x) 2^x,
      requires = quote(x1 == "b")),
    makeDiscreteParam("x5", values = c("linear", "radial"),
      requires = quote(x1 == "b")),
    makeNumericParam("x6", lower = -15L, upper = 15L, trafo = function(x) 2^x,
      requires = quote(x1 == "b" & x5 == "radial")),
    
    makeIntegerParam("x7", lower = 1L, upper = 21L,
      requires = quote(x1 == "c"))
  )
  
  des = generateDesignOversampling(13L, par.set, oversampling = 10L)
  
  # A how lot of tests ...
  expect_equal(nrow(des), 13L)
  expect_equal(ncol(des), 7L)
  expect_equal(colnames(des), paste("x", 1:7, sep = ""))
  expect_true(is.factor(des[, 1L]))
  expect_true(is.integer(des[, 2L]))
  expect_true(is.integer(des[, 3L]))
  expect_true(is.numeric(des[, 4L]))
  expect_true(is.factor(des[, 5L]))
  expect_true(is.numeric(des[, 6L]))
  expect_true(is.integer(des[, 7L]))
  expect_true(all(des[, 1L] %in% c("a", "b", "c")))
  expect_true(all(is.na(des[, 2L]) | des[, 2L] >= 15 & des[, 2L] <= 45))
  expect_true(all(des[, 1L] == "a" | is.na(des[, 2L])))
  expect_true(all(is.na(des[, 3L]) | des[, 3L] >= 1 & des[, 3L] <= 120))
  expect_true(all(des[, 1L] == "a" | is.na(des[, 3L])))
  expect_true(all(is.na(des[, 4L]) | des[, 4L] >= -15 & des[, 4L] <= 15))
  expect_true(all(des[, 1L] == "b" | is.na(des[, 4L])))
  expect_true(all(is.na(des[, 5L]) | des[, 5L] %in% c("linear", "radial")))
  expect_true(all(des[, 1L] == "b" | is.na(des[, 5L])))
  expect_true(all(is.na(des[, 6L]) | des[, 6L] >= -15 & des[, 6L] <= 15))
  expect_true(all(des[, 1L] == "b" & des[, 5L] == "radial" | is.na(des[, 6L])))
  expect_true(all(is.na(des[, 7L]) | des[, 7L] >= 1 & des[, 7L] <= 21))
  expect_true(all(des[, 1L] == "c" | is.na(des[, 7L])))
  
  
  # Does start.design work?
  start.design = generateDesign(2L, par.set)
  des = generateDesignOversampling(13L, par.set, oversampling = 10L, start.design = start.design)
  
  expect_equal(des[1, ], start.design[1, ])
  expect_equal(des[2, ], start.design[2, ])
  
})
