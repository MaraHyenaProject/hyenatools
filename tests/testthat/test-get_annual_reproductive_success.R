# get_annual_reproductive_success() warns when hyenadata is not attached. We
# attach it (data still comes from the fixture) so the only warnings under test
# are the function's own (non-female ids, missing ids). See helper-fixtures.R
# for the fixture layout.

setup_ars <- function() {
  skip_if_not_installed("hyenadata")
  suppressMessages(suppressWarnings(require(hyenadata, quietly = TRUE)))
  skip_if_not("hyenadata" %in% names(sessionInfo()$otherPkgs),
              "hyenadata not attachable")
  use_ars_fixtures()
}

test_that("counts offspring born per year for a focal female", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_annual_reproductive_success("f1", end_year = 2010)

  expect_setequal(names(res),
                  c("id", "year", "offspring_born", "offspring_surv_to_2"))
  expect_true(all(res$id == "f1"))
  expect_equal(sum(res$offspring_born), 3)
  expect_equal(res$offspring_born[res$year == 2003], 2)  # c1 + c2
  expect_equal(res$offspring_born[res$year == 2005], 1)  # c3
})

test_that("years with no births are reported as zero", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_annual_reproductive_success("f1", end_year = 2010)

  expect_equal(res$offspring_born[res$year == 2004], 0)
  expect_equal(res$offspring_surv_to_2[res$year == 2004], 0)
})

test_that("survival to age 2 counts only cubs that lived >=2y or have no end date", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_annual_reproductive_success("f1", end_year = 2010)

  # 2003: c1 survived (~3y), c2 did not (~4m) -> 1 of 2
  expect_equal(res$offspring_surv_to_2[res$year == 2003], 1)
  # 2005: c3 has no disappearance date -> counted as survived
  expect_equal(res$offspring_surv_to_2[res$year == 2005], 1)
  expect_equal(sum(res$offspring_surv_to_2), 2)
})

test_that("the reproductive window ends at end_year", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_annual_reproductive_success("f1", end_year = 2010)

  expect_equal(max(res$year), 2010)
})

test_that("a female that died before age 2 contributes no rows", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_annual_reproductive_success("f2", end_year = 2010)

  expect_equal(nrow(res), 0)
})

test_that("non-female ids warn and produce zero offspring", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(
    res <- get_annual_reproductive_success("m1", end_year = 2010),
    "not female"
  )
  expect_equal(sum(res$offspring_born), 0)
})

test_that("ids absent from tblHyenas warn and are dropped", {
  setup_ars()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(
    res <- get_annual_reproductive_success("ghost"),
    "not in tblHyenas"
  )
  expect_equal(nrow(res), 0)
})
