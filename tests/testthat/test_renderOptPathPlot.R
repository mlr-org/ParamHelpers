context("renderOptPathPlot")

test_that("renderOptPathPlot", {
  # Test 1D-1D + title
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
  plotOptPath(op0, iters = 0:2, pause = FALSE, title = "Optimization Path")
  
  # Test 2D-2D + title
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
  plotOptPath(op1, iters = 0:2, pause = FALSE, title = "Optimization Path")
  plotOptPath(op1, iters = 0:2, pause = FALSE,
    lim.x = list(XSpace = c(-10, 10), YSpace = c(-10, 10)),
    lim.y = list(XSpace = c(-10, 10), YSpace = c(-10, 10))
  )
  
  # Test 2D-3D + size
  op2 = makeOptPathDF(par.set = ps1, y.names = c("y1", "y2", "y3"), minimize = c(TRUE, FALSE, TRUE))
  X = rnorm(35)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op2, x = list(x = X[i * 5 - 4], z = X[i * 5 - 3]),
      y = c(y1 = X[i * 5 - 2], y2 = X[i * 5 - 1], y3 = X[i * 5]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op2, iter = 0)
  plotOptPath(op2, iters = 0:2, pause = FALSE, size = c(5, 3))
  
  # Test 3D-3D
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
  
  # Test 3D-1D
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
  
  # Test 1D(discrete)-1D
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
  
  # Test 2D(mixed)-1D
  ps6 = makeParamSet(
    makeNumericParam("x"),
    makeDiscreteParam("z", values = list("a", "b"))
  )
  op6 = makeOptPathDF(par.set = ps6, y.names = c("y1"), minimize = c(TRUE))
  X = rep(c("a", "b"), 4)
  Y = rnorm(14)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:6) {
    addOptPathEl(op6, x = list(x = Y[i], z = X[i]), 
      y = c(y1 = Y[i + 7]), dob = dob[i])
  }
  addOptPathEl(op6, x = list(x = Y[6] + 0.05, z = X[6]), 
      y = c(y1 = Y[6]), dob = dob[7])
  pl <- renderOptPathPlot(op6, iter = 0)
  plotOptPath(op6, iters = 0:2, pause = FALSE)
  
  
  # Test 2D(discrete)-1D
  ps7 = makeParamSet(
    makeDiscreteParam("x", values = list("a", "b")),
    makeDiscreteParam("z", values = list("c", "d"))
  )
  op7 = makeOptPathDF(par.set = ps7, y.names = c("y1"), minimize = c(TRUE))
  X1 = rep(c("a", "b"), 4)
  X2 = rep(c("c", "d"), each = 4)
  Y = rnorm(7)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:6) {
    addOptPathEl(op7, x = list(x = X1[i], z = X2[i]), 
      y = c(y1 = Y[i]), dob = dob[i])
  }
  addOptPathEl(op7, x = list(x = X1[6], z = X2[6]), 
    y = c(y1 = Y[7]), dob = dob[7])
  pl <- renderOptPathPlot(op7, iter = 0)
  plotOptPath(op7, iters = 0:2, pause = FALSE)
  
  # Test 3D(mixed)-1D + colours
  ps8 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("y"),
    makeDiscreteParam("z", values = list("a", "b", "c"))
  )
  op8 = makeOptPathDF(par.set = ps8, y.names = c("y1"), minimize = c(TRUE))
  X1 = rnorm(14)
  X2 = rep(c("a", "b", "c"), 5)
  Y = rnorm(7)
  dob = c(rep(0, 5), 1:2)
  for (i in 1:7) {
    addOptPathEl(op8, x = list(x = X1[i], y = X1[i + 7], z = X2[i]), 
      y = c(y1 = Y[i]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op8, iter = 0)
  plotOptPath(op8, iters = 0:2, pause = FALSE, colours = c("black", "yellow", "orange"))
  
})
  