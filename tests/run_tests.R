#!/usr/bin/env Rscript
# Convenience runner for the hyenatools test suite during development.
#
# Run from the package root with:
#   Rscript tests/run_tests.R
#
# This uses devtools::test(), which loads the package with load_all() and runs
# the tests in tests/testthat/ against that namespace -- no install required.
# `R CMD check` uses tests/testthat.R (test_check()) instead.

suppressMessages(library(devtools))
devtools::test(stop_on_failure = TRUE)
