# alpha [1, 1.4] [1.189]l
# rho [0, 1] [0.5]
# ps [0, 0.2] [0.1]
# wp [0, 0.06] [0.03]

ps = makeParamSet(
  makeNumericParam("alpha", lower = -5, upper = 10, default = 10),
  makeNumericParam("rho", lower = 0, upper = 15, default = 15),
  makeNumericParam("ps", lower = 0, upper = 15, default = 15),
  makeNumericParam("wp", lower = 0, upper = 15, default = 15)
)

