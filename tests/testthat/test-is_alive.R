# is_alive() warns when hyenadata is not attached; we attach it (data still comes
# from the fixture). Reuses the ARS fixture (see helper-fixtures.R):
#   f1, f2 have disappearance dates; m1, f3 do not.

setup_is_alive <- function() {
  skip_if_not_installed("hyenadata")
  suppressMessages(suppressWarnings(require(hyenadata, quietly = TRUE)))
  skip_if_not("hyenadata" %in% names(sessionInfo()$otherPkgs),
              "hyenadata not attachable")
  use_ars_fixtures()
}

test_that("a hyena with a disappearance date is not alive", {
  setup_is_alive()
  on.exit(clear_fixtures(), add = TRUE)

  expect_false(is_alive("f1"))
})

test_that("a hyena with no disappearance date is alive", {
  setup_is_alive()
  on.exit(clear_fixtures(), add = TRUE)

  expect_true(is_alive("f3"))
  expect_true(is_alive("m1"))
})

test_that("is vectorized over ids and preserves order", {
  setup_is_alive()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(is_alive(c("f1", "f3", "f2")), c(FALSE, TRUE, FALSE))
})

test_that("ids not in tblLifeHistory.wide warn and return NA", {
  setup_is_alive()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(res <- is_alive("ghost"), "not in tblLifeHistory.wide")
  expect_true(is.na(res))
})
