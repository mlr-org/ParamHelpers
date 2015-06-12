context("plotYTrace")

test_that("plotYTrace works", {
  
  makeArtOP = function() {
    ps = makeNumericParamSet(len = 1L)
    op = makeOptPathDF(par.set = ps, y.names = "y",
      minimize = TRUE)
    X = rnorm(40)
    dob = c(rep(0, 5), rep(1:7, each = 2), 8)
    for (i in 1:20) {
      addOptPathEl(op, x = list(
        x = X[i * 2]),
        y = X[ i * 2 - 1],
        dob = dob[i])
    }
    op
  }
  # build aritificial list of opt pathes
  opt.paths = list(
    algo1 = makeArtOP(),
    algo2 = makeArtOP(),
    algo3 = makeArtOP(),
    algo4 = makeArtOP())
  
  
  pl = renderYTraces(opt.paths)
  expect_is(pl, "gg")
  expect_is(pl, "ggplot")
  plotYTraces(opt.paths)
})