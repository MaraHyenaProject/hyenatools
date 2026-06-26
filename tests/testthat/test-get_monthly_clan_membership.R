# NOTE: get_monthly_clan_membership() is currently broken. Inside its main loop
# it references an undefined variable `y` (it should be using dates$year[i], as
# annual_clan_membership() does). This test documents the current behaviour so
# that fixing the bug will flag here. Once `y` is replaced with the correct
# per-iteration year, replace this with real behavioural assertions.

test_that("requires some date information", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_error(
    suppressWarnings(get_monthly_clan_membership(clans = "talek")),
    "No date info provided"
  )
})

test_that("supplying both dates and months/years is rejected", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_error(
    suppressWarnings(get_monthly_clan_membership(
      clans = "talek",
      dates = as.Date("2011-06-01"),
      months = 6, years = 2011
    )),
    "either months/years or dates"
  )
})

test_that("KNOWN BUG: errors on undefined variable `y` during filtering", {
  use_clan_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  # Expected to fail with "object 'y' not found" until the bug is fixed.
  expect_error(
    suppressWarnings(get_monthly_clan_membership(
      clans = "talek", months = 6, years = 2011
    )),
    "'y' not found"
  )
})
