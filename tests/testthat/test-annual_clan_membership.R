# annual_clan_membership() warns when the hyenadata package is not attached.
# We use the global-environment fixture and suppress that incidental warning.

test_that("returns members present in the clan during the requested year", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(annual_clan_membership(clans = "talek", years = 2011))

  expect_s3_class(res, "data.frame")
  expect_setequal(res$id, c("h1", "h2"))
  expect_true(all(res$year == 2011))
})

test_that("a still-present member (NA end_date) is included in later years", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(annual_clan_membership(clans = "talek", years = 2013))

  # h1 left in 2012; h2 has no end_date so is still a member
  expect_setequal(res$id, "h2")
})

test_that("members who left before the year are excluded", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(annual_clan_membership(clans = "talek", years = 2009))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("a year with members and a year without combine cleanly", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  # 2009 has no talek members, 2011 has h1 and h2
  res <- suppressWarnings(annual_clan_membership(clans = "talek", years = c(2009, 2011)))

  expect_setequal(res$id, c("h1", "h2"))
  expect_true(all(res$year == 2011))
})

test_that("dispersal/fission notes are kept in their start year", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(annual_clan_membership(clans = "serena.n", years = 2009))

  expect_equal(res$id, "h3")
  expect_equal(res$notes, "dispersal")
})

test_that("dispersal/fission notes are cleared in subsequent years", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(annual_clan_membership(clans = "serena.n", years = 2010))

  expect_equal(res$id, "h3")
  expect_equal(res$notes, "")
})

test_that("multiple years are returned together, one row per year of membership", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(
    annual_clan_membership(clans = "talek", years = 2011:2012)
  )

  expect_setequal(unique(res$year), c(2011, 2012))
})
