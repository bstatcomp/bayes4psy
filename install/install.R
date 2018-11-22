library(devtools)
library(roxygen2)
options(devtools.install.args = "--no-multiarch")

roxygen2::roxygenise(clean=TRUE)
devtools::document()
devtools::install(local = TRUE)

