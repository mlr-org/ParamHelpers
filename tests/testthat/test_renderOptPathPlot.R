context("renderOptPathPlot")

test_that("renderOptPathPlot", {
  # 1D-1D
  ps0 = makeParamSet(
    makeNumericParam("x")
  )
  op0 = makeOptPathDF(par.set = ps0, y.names = c("y"), minimize = TRUE)
  X = rnorm(14)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op0, x = list(
      x = X[i * 2]),
      y = X[ i * 2 - 1],
      dob = dob[i])
  }
  pl <- renderOptPathPlot(op0, iter = 0)
  plotOptPath(op0, iters = 0:2, pause = FALSE)
  
  # 2D-2D
  ps1 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("z")
  )
  op1 = makeOptPathDF(par.set = ps1, y.names = c("y1", "y2"), minimize = c(TRUE, FALSE))
  X = rnorm(28)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op1, x = list(
      x = X[i * 4 - 3], z = X[i * 4 - 2]),
      y = c(y1 = X[ i * 4 - 1], y2 = X[i * 4]),
      dob = dob[i])
  }
  pl <- renderOptPathPlot(op1, iter = 0)
  plotOptPath(op1, iters = 0:2, pause = FALSE)
  plotOptPath(op1, iters = 0:2, pause = FALSE,
    lim.x = list(XSpace = c(-10, 10), YSpace = c(-10, 10)),
    lim.y = list(XSpace = c(-10, 10), YSpace = c(-10, 10))
      )
  
  # 2D-3D
  op2 = makeOptPathDF(par.set = ps1, y.names = c("y1", "y2", "y3"), minimize = c(TRUE, FALSE, TRUE))
  X = rnorm(35)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op2, x = list(x = X[i * 5 - 4], z = X[i * 5 - 3]),
      y = c(y1 = X[i * 5 - 2], y2 = X[i * 5 - 1], y3 = X[i * 5]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op2, iter = 0)
  plotOptPath(op2, iters = 0:2, pause = FALSE)
  
  # 3D-3D
  ps3 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("z"), 
    makeNumericParam("z2")
  )
  op3 = makeOptPathDF(par.set = ps3, y.names = c("y1", "y2", "y3"), minimize = c(TRUE, FALSE, TRUE))
  X = rnorm(42)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op3, x = list(x = X[i * 6 - 5], z = X[i * 6 - 4], z2 = X[i * 6 - 3]) , 
      y = c(y1 = X[i * 6 - 2], y2 = X[i * 6 - 1], y3 = X[i * 6]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op3, iter = 0)
  plotOptPath(op3, iters = 0:2, pause = FALSE)
  
  # 3D-1D
  ps4 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("z"), 
    makeNumericParam("z2")
  )
  op4 = makeOptPathDF(par.set = ps4, y.names = c("y1"), minimize = c(TRUE))
  X = rnorm(28)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op4, x = list(x = X[i * 4 - 3], z = X[i * 4 - 2], z2 = X[i * 4 - 1]), 
                 y = c(y1 = X[i * 4]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op4, iter = 0)
  plotOptPath(op4, iters = 0:2, pause = FALSE)
  
  # 1D(discrete)-1D
  ps5 = makeParamSet(
    makeDiscreteParam("x", values = list("a", "b"))
  )
  op5 = makeOptPathDF(par.set = ps5, y.names = c("y"), minimize = TRUE)
  X = rep(c("a", "b"), 4)
  Y = rnorm(7)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op5, x = list(
      x = X[i]),
      y = Y[i],
      dob = dob[i])
  }
  pl <- renderOptPathPlot(op5, iter = 0)
  plotOptPath(op5, iters = 0:2, pause = FALSE)
})
  