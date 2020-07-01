library(testthat)
context("modifyParam")

test_that("modifyParam properly modifies parameter limits/values", {
   
    ps = makeParamSet(
    makeIntegerParam("power", lower = 10, upper = 5555),
    makeIntegerParam("time", lower = 500, upper = 20210),
    makeDiscreteParam("gas", values = c("Nitrogen", "Air", "Argon")),
    makeIntegerParam("pressure", lower = 20, upper = 1000)
    )
    
    # Normal operation - correct limits, values and ids
    
    # set parameter to one value 
    ms = modifyParam(ps, id="power", lower = 444, upper = 444)
    expect_equal(ms[["pars"]][["power"]][["lower"]], 444)
    expect_equal(ms[["pars"]][["power"]][["upper"]], 444)
    
    # set parameter to range of values
    ms = modifyParam(ps, id="power", lower = 444, upper = 555)
    expect_equal(ms[["pars"]][["power"]][["lower"]], 444)
    expect_equal(ms[["pars"]][["power"]][["upper"]], 555)
    
    # A number of ways to set values for a discrete parameter
    ms = modifyParam(ps, id="gas", lower = "Argon")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Argon"]], "Argon")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]])
    expect_null(ms[["pars"]][["gas"]][["values"]][["Air"]])
    
    ms = modifyParam(ps, id="gas", lower = list("Argon", "Nitrogen"))
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Argon"]], "Argon")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]], "Nitrogen")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Air"]])
    
    ms = modifyParam(ps, id="gas", upper = list("Air", "Argon"))
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Argon"]], "Argon")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Air"]], "Air")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]])
    
    ms = modifyParam(ps, id="gas", lower = "Air", upper = "Nitrogen")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]], "Nitrogen")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Air"]], "Air")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Argon"]])
    
    # Incorrect/Invalid/Outside-limit  values/limits/ids
    
    # if no upper/lower value given, defaults to original value from ps
    ms = modifyParam(ps, id="power", lower = 444)
    expect_equal(ms[["pars"]][["power"]][["lower"]], 444)
    expect_equal(ms[["pars"]][["power"]][["upper"]], 5555)
    ms = modifyParam(ps, id="power", upper = 444)
    expect_equal(ms[["pars"]][["power"]][["lower"]], 10)
    expect_equal(ms[["pars"]][["power"]][["upper"]], 444)
    
    # if upper/lower outside limits of ps, set to ps limits
    ms = modifyParam(ps, id="pressure", lower = -3, upper = 444)
    expect_equal(ms[["pars"]][["pressure"]][["lower"]], 20)
    expect_equal(ms[["pars"]][["pressure"]][["upper"]], 444)
    ms = modifyParam(ps, id="pressure", lower = 333, upper = 22000)
    expect_equal(ms[["pars"]][["pressure"]][["lower"]], 333)
    expect_equal(ms[["pars"]][["pressure"]][["upper"]], 1000)
    ms = modifyParam(ps, id="pressure", lower = 12, upper = 3)
    expect_equal(ms[["pars"]][["pressure"]][["lower"]], 20)
    expect_equal(ms[["pars"]][["pressure"]][["upper"]], 1000)
    
    # if non-existant value given for discrete param use value set from ps
    ms = modifyParam(ps, id="gas", lower = "Airrr")
    expect_equal(ms[["pars"]][["gas"]][["values"]], ps[["pars"]][["gas"]][["values"]])
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]], "Nitrogen")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Air"]], "Air")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Argon"]], "Argon")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Airrr"]])
    
    # if no value given for discrete param use those from ps
    ms = modifyParam(ps, id="gas")
    expect_equal(ms[["pars"]][["gas"]][["values"]], ps[["pars"]][["gas"]][["values"]])
    
    # if non-existant value given for discrete param use value set from ps
    ms = modifyParam(ps, id="gas", lower = "Nitrogen", upper = "Arrgon")
    expect_equal(ms[["pars"]][["gas"]][["values"]], ps[["pars"]][["gas"]][["values"]])
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]], "Nitrogen")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Air"]], "Air")
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Argon"]], "Argon")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Arrgon"]])
    
    # if non-existant value given for discrete param in a list ignore that value
    ms = modifyParam(ps, id="gas", lower = list("Air","Arrgon"))
    expect_equal(ms[["pars"]][["gas"]][["values"]][["Air"]], "Air")
    expect_null(ms[["pars"]][["gas"]][["values"]][["Nitrogen"]])
    expect_null(ms[["pars"]][["gas"]][["values"]][["Argon"]])
    expect_null(ms[["pars"]][["gas"]][["values"]][["Arrgon"]])
    
    # if param id does not exist in ps, returns ps
    ms = modifyParam(ps, id="powwer", lower = 444, upper = 54321)
    expect_equal(ps, ms)
    ms = modifyParam(ps, id="ggas", lower = list("Argon", "Nitrogen"))
    expect_equal(ps, ms)
    
    # if parameter value is of wrong type, sets to ps values
    ms = modifyParam(ps, id="power", lower = "abc", upper = 5555)
    expect_equal(ps, ms)
    ms = modifyParam(ps, id="gas", lower = 444)
    expect_equal(ps, ms)
    
    


})
