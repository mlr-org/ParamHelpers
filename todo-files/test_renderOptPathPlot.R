context("renderOptPathPlot")

test_that("renderOptPathPlot", {
  # 1D-1D
  ps0 = makeParamSet(
    makeNumericParam("x")
  )
  op0 = makeOptPathDF(par.set = ps0, y.names = c("y"), minimize = TRUE)
  X = rnorm(20)
  dob = c(rep(0, 5), 1:5)
  for (i in 1:10) {
    addOptPathEl(op0, x = list(
      x = X[i * 2]),
      y = X[ i * 2 - 1],
      dob = dob[i])
  }
  pl <- renderOptPathPlot(op0, iters = 1:5)
  
  
  # 2D-2D
  ps1 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("z")
  )
  op1 = makeOptPathDF(par.set = ps1, y.names = c("y1", "y2"), minimize = c(TRUE, FALSE))
  X = rnorm(40)
  dob = c(rep(0, 5), 1:5)
  for (i in 1:10) {
    addOptPathEl(op1, x = list(
      x = X[i * 4 - 3], z = X[i * 4 - 2]),
      y = c(y1 = X[ i * 4 - 1], y2 = X[i * 4]),
      dob = dob[i])
  }
  pl <- renderOptPathPlot(op1, iters = 1:5)
  
  
  
  # 2D-3D
  op2 = makeOptPathDF(par.set = ps1, y.names = c("y1", "y2", "y3"), minimize = c(TRUE, FALSE, TRUE))
  X = rnorm(50)
  dob = c(rep(0, 5), 1:5)
  for (i in 1:10) {
    addOptPathEl(op2, x = list(x = X[i * 5 - 4], z = X[i * 5 - 3]),
      y = c(y1 = X[i * 5 - 2], y2 = X[i * 5 - 1], y3 = X[i * 5]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op2, iters = 1:5)
  
  # 3D-3D
  ps3 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("z"), 
    makeNumericParam("z2")
  )
  op3 = makeOptPathDF(par.set = ps3, y.names = c("y1", "y2", "y3"), minimize = c(TRUE, FALSE, TRUE))
  X = rnorm(60)
  dob = c(rep(0, 5), 1:5)
  for (i in 1:10) {
    addOptPathEl(op3, x = list(x = X[i * 6 - 5], z = X[i * 6 - 4], z2 = X[i * 6 - 3]) , 
      y = c(y1 = X[i * 6 - 2], y2 = X[i * 6 - 1], y3 = X[i * 6]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op3, iters = 1:5)
  
  # 3D-1D
  ps4 = makeParamSet(
    makeNumericParam("x"),
    makeNumericParam("z"), 
    makeNumericParam("z2")
  )
  op4 = makeOptPathDF(par.set = ps4, y.names = c("y1"), minimize = c(TRUE))
  X = rnorm(40)
  dob = c(rep(0, 5), 1:5)
  for (i in 1:10) {
    addOptPathEl(op4, x = list(x = X[i * 4 - 3], z = X[i * 4 - 2], z2 = X[i * 4 - 1]), 
                 y = c(y1 = X[i * 4]), dob = dob[i])
  }
  pl <- renderOptPathPlot(op4, iters = 1:5)
  
})
  