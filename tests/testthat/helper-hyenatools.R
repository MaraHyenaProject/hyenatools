# Test setup for hyenatools
#
# The package cannot currently be loaded with devtools::load_all() / installed,
# because two work-in-progress files (R/get_ars.R and R/get_maternal_descendanys.R)
# contain invalid R syntax. Until those are finished, these tests source the
# individual function files directly instead of loading the package namespace.
#
# Every R/*.R file that parses successfully is sourced into the global
# environment. As soon as the WIP files are fixed they will be picked up
# automatically (and this whole file can be replaced by a normal
# testthat::test_check() / load_all() setup).

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# Locate the package root relative to the testthat working directory.
.pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                           mustWork = TRUE)
.r_dir <- file.path(.pkg_root, "R")

# Source every R file that parses; report (but tolerate) the ones that don't.
for (f in list.files(.r_dir, pattern = "\\.R$", full.names = TRUE)) {
  parsed <- tryCatch(parse(f), error = function(e) e)
  if (inherits(parsed, "error")) {
    message("Skipping unparseable file: ", basename(f),
            " (", conditionMessage(parsed), ")")
    next
  }
  source(f, local = FALSE)  # define functions in the global environment
}
