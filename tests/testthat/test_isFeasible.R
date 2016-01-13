context("isFeasible")

test_that("requieres work", {
  ps1 = makeParamSet(
    makeDiscreteParam("A", list(a = "1", b = "2"), default = "1"),
    makeDiscreteParam("B", list('0' = "z", '1' = "a"), requires = quote(A == "1"), default = "z")
  )
  b = filterParams(ps1, ids = "B")
  #requiresOk(b$pars$B, x = b)
})
