# get_lifetime_reproductive_success() warns when hyenadata is not attached. We
# attach it (data still comes from the fixture) so the only warnings under test
# are the function's own. Reuses the ARS fixture (see helper-fixtures.R):
#   f1 has 3 offspring, of which 2 survived to age 2 (c1 and c3; c2 died young).

setup_lrs <- function() {
  skip_if_not_installed("hyenadata")
  suppressMessages(suppressWarnings(require(hyenadata, quietly = TRUE)))
  skip_if_not("hyenadata" %in% names(sessionInfo()$otherPkgs),
              "hyenadata not attachable")
  use_ars_fixtures()
}

test_that("totals offspring produced and those surviving to age 2", {
  setup_lrs()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_lifetime_reproductive_success("f1")

  expect_setequal(names(res), c("id", "offspring_born", "lrs"))
  expect_equal(nrow(res), 1)
  expect_equal(res$offspring_born, 3)
  expect_equal(res$lrs, 2)
})

test_that("a female with no offspring gets a zero row (not dropped)", {
  setup_lrs()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_lifetime_reproductive_success("f2")

  expect_equal(nrow(res), 1)
  expect_equal(res$offspring_born, 0)
  expect_equal(res$lrs, 0)
})

test_that("multiple ids return one row each, in order", {
  setup_lrs()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_lifetime_reproductive_success(c("f1", "f2"))

  expect_equal(res$id, c("f1", "f2"))
  expect_equal(res$lrs, c(2, 0))
})

test_that("non-female ids warn and produce zero offspring", {
  setup_lrs()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(
    res <- get_lifetime_reproductive_success("m1"),
    "not female"
  )
  expect_equal(res$offspring_born, 0)
  expect_equal(res$lrs, 0)
})

test_that("ids absent from tblHyenas warn and are dropped", {
  setup_lrs()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(
    res <- get_lifetime_reproductive_success("ghost"),
    "not in tblHyenas"
  )
  expect_equal(nrow(res), 0)
})
