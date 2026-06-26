#!/usr/bin/env Rscript
# Convenience runner for the hyenatools test suite.
#
# Run from the package root with:
#   Rscript tests/run_tests.R
#
# We use testthat::test_dir() rather than the usual test_check()/load_all()
# flow because two work-in-progress files (R/get_ars.R,
# R/get_maternal_descendanys.R) currently contain invalid R and prevent the
# package from being loaded or installed. The test helpers source the valid
# function files directly. Once those WIP files are finished, this can be
# replaced with a standard testthat setup.

suppressMessages(library(testthat))
testthat::test_dir("tests/testthat", reporter = "progress", stop_on_failure = TRUE)
