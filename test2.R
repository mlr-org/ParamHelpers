load_all()

ps = makeParamSet(
    makeNumericParam("u", lower = 1),
    makeIntegerParam("v", lower = 1, upper = 2),
    makeDiscreteParam("w", values = 1:2),
    makeLogicalParam("x"),
    makeDiscreteVectorParam("y", len = 2, values = c("a", "b")),
    makeLogicalVectorParam("z", len = 3),
    makeCharacterVectorParam("s", len = 2)
  )
  print(getParamIds(filterParams(ps, type = "numeric")))

  # expect_equal(getParamIds(filterParams(ps, type = "integer")), "v")
  # expect_equal(getParamIds(filterParams(ps, type = "discrete")), "w")
  # expect_equal(getParamIds(filterParams(ps, type = "logical")), "x")
  # expect_equal(getParamIds(filterParams(ps, type = c("logical", "logicalvector"))), c("x", "z"))
  # expect_equal(getParamIds(filterParams(ps, type = c("character", "charactervector"))), "s")
  # expect_equal(getParamIds(filterParams(ps, type = "discretevector")), "y")
  # expect_equal(getParamIds(filterParams(ps, type = c("numeric","integer"))), c("u", "v"))
  # expect_equal(getParamIds(filterParams(ps, type = c("integer","numeric"))), c("u", "v"))
  # expect_equal(getParamIds(filterParams(ps, type = c("integer","function"))), "v")
  # expect_true(isEmpty(filterParams(ps, type = "function")))

