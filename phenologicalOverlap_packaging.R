library(devtools)
setwd("~/Data/aktuell/bipartite/phenologicalOverlap")
document() # process R-functions into .RD files, change namespace
setwd("..")
build("phenologicalOverlap", path="phenologicalOverlap")   # build the .tar.gz file
install("phenologicalOverlap") # install on the computer


# write a vignette:
devtools:::use_vignette("phenologicalOverlap-vignette")


# test things:

library(phenologicalOverlap)


# issues
* the .ltx-vignette does not build
* the vignettes are not shown in the package-overview of RStudio (or elsewhere)


# wishlist
* real data to play with in the vignette
* for testthat: analytical solutions and known results cases for verification of code
* documentation of available metrics in vignette