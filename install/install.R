options(devtools.install.args = "--no-multiarch")

pkgbuild::compile_dll()

roxygen2::roxygenise(clean=TRUE)
devtools::install(local=TRUE)
devtools::document()
