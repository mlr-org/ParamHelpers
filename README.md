ParamHelpers
============

Miscellaneous helper functions for and from B. Bischl and some other guys at TU Dortmund, mainly for package development.

Offical CRAN release site: 
http://cran.r-project.org/web/packages/ParamHelpers/index.html

R Documentation in HTML:
http://www.statistik.tu-dortmund.de/~bischl/rdocs/ParamHelpers/

Installation
============

1) Normal users:
Please use the CRAN version linked above.

2) Early adopters: Simply running
```r
install_github("ParamHelpers", username="berndbischl")
```
will install the current github version.

3) Assuming you have a reasonably configured OS and R, you could also build and run tasks via the MAKEFILE.
But only a VERY SMALL percentage of users should be interested in this, instead of options 1) or 2).

- Clone from git repo here

- Have recent version of R properly installed with all build tools. For Windows this will include 
  
  http://cran.r-project.org/bin/windows/Rtools/

- Have git as a command-line tool available.

- Have roxygen2, devtools and testhat R packages installed

- In a console run "make install" to install. Done.

- "make" will list all other build targets

- If you have problems (e.g. in Windows) because there is no "git" command line
  tool that we use to figure out the build number, remove these lines from the Makefile:
  
```
  echo "Setting version ...
  ${RSCRIPT} ./tools/set-version
```
