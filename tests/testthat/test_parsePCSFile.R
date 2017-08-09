context("parsePCSFile")

test_that("parsePCSFile ", {
  path = system.file("inst/pcs_files", package = "ParamHelpers")
  fns = list.files(path, pattern = ".pcs", full.names = TRUE)

   #FIXME: loop over all!


  # read a pcs file, trim WS, and remove all empty lines
  readPCSLines = function(path) {
    lines = stri_read_lines(fn)
    lines = vcapply(lines, str_trim)
    lines[nzchar(lines)]
  }


  for (fn in fns[1]) {
    # print(fn)
    # parse normal PCS files
    # ps = parsePCSFile(fn)
    # parse mathing R file into envir
    fnr = stri_replace(fn, fixed = ".pcs", ".R")
    # print(fnr)
    ee = new.env()
    source(fnr, local = ee)

    # check that parsed result matches manual paramset R file
    # expect_equal(ps, ee$ps, info = sprintf("pcs parser: %s", fn))

    # check that writer works
    outpath = tempfile()
    writePCSFile(ee$ps, outpath)
    lines1 = readPCSLines(fn)
    # print(lines1)
    lines2 = readPCSLines(outpath)
    # print(lines2)
    expect_equal(lines1, lines2, info = sprintf("pcs writer: %s", fnr))
  }
})
