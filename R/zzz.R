# if (getRversion() >= '2.15.1') {
#   utils::globalVariables(c())
# }

.onAttach <- function(libname, pkgname) {
  ## work around since these are github packages only
  ok <- function(x)
    tryCatch(find.package(x), error = function(e) invisible(TRUE))
  
  if (isTRUE(ok('rawr')))
    packageStartupMessage("\'rawr\' package may be needed:\n\n",
                          "run devtools::install_github('raredd/rawr')",
                          domain = NA)
  
  invisible(NULL)
}
