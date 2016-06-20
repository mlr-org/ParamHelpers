load_all()

ps = makeParamSet(
  makeNumericVectorParam("test1", len=2, -10, 10),
  makeDiscreteParam("testDiscrete", c("a", "b")),
  makeNumericVectorParam("test3", len=2, -10, 10)
)

x = convertParamSetToIrace(ps)$boundary
# print(x)

