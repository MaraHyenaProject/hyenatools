# get_clan_status() warns when the hyenadata package is not attached; we use the
# global-environment fixture and suppress that incidental warning.

test_that("returns clan and status for a date within a membership interval", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(get_clan_status("h1", "2011-06-01"))

  expect_equal(nrow(res), 1)
  expect_equal(res$clan, "talek")
  expect_equal(res$status, "natal")
})

test_that("an open-ended membership (NA end_date) is treated as ongoing", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(get_clan_status("h2", "2024-01-01"))

  expect_equal(res$clan, "talek")
  expect_equal(res$status, "resident")
})

test_that("a date outside any interval yields NA clan/status", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  # h2 only joined talek in 2011-03; before that it has no membership row
  res <- suppressWarnings(get_clan_status("h2", "2010-01-01"))

  expect_true(is.na(res$clan))
  expect_true(is.na(res$status))
})

test_that("vectorized ids and dates are matched element-wise", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(
    get_clan_status(c("h1", "h3"), c("2011-06-01", "2009-06-01"))
  )

  expect_equal(nrow(res), 2)
  expect_equal(res$clan[res$ids == "h1"], "talek")
  expect_equal(res$clan[res$ids == "h3"], "serena.n")
})

test_that("ids absent from tblClanMembership warn and return NA", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(
    res <- get_clan_status("ghost", "2011-06-01"),
    "missing from tblClanMembership"
  )
  expect_true(is.na(res$clan))
})
