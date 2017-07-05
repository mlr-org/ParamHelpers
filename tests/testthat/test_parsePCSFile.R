context("parsePCSFile")

test_that("parsePCSFile ", {
  path = system.file("inst/pcs_files", package = "ParamHelpers")
  fns = list.files(path)

   #FIXME: loop over all!
  for (fn in fns[1]) {
    # parse normal PCS files
    ps = parsePCSFile(fn)
    # parse mathing R file into envir
    fnr = stri_replace(fn, fixed = ".pcs", ".R")
    ee = new.env()
    source(fnr, local = ee)
    expect_equal(ps, ee$ps)
  }
})
