load_all()
options(warn = 1)

# Test 2D(mixed)-3D + marked + ggplot.theme + log + size
ps1 = makeParamSet(
  makeNumericParam("x"),
  makeDiscreteParam("z", values = list("a", "b"))
  )
op1 = makeOptPathDF(par.set = ps1, y.names = c("y1", "y2", "y3"), minimize = c(TRUE, FALSE, TRUE))
X = rnorm(100)
Z = rnorm(25)
Z = ifelse(Z < 0, "a", "b")
dob = c(rep(0, 5), 1:20)
for (i in 1:25) {
  addOptPathEl(op1, x = list(x = abs(X[i * 4 - 3]), z = Z[i]),
    y = c(y1 = X[i * 4 - 2], y2 = abs(X[i * 4 - 1]), y3 = abs(X[i * 4])), dob = dob[i])
}
# pl = renderOptPathPlot(op1, iter = 3)
plotOptPath(op1)
