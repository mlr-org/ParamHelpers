x0 [-3,3] [3]
x1 [-2, 2] [2]

ps = makeParamSet(
  makeNumericParam("x0", lower = -3, upper = 3, default = 3),
  makeNumericParam("x1", lower = -2, upper = 2, default = 2)
)

