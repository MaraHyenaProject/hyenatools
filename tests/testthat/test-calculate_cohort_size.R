# calculate_cohort_size() reads its tables from the global environment; the
# cohort fixture (see helper-fixtures.R) is built so the expected cohort of the
# focal cub "a" is exactly {a, b}. It warns when hyenadata isn't attached, so we
# suppress warnings on normal calls.

cohort_members <- function(res, id) {
  sort(strsplit(res$cohort[res$id == id], ",")[[1]])
}

test_that("cohort = same-clan cubs with enough den overlap that were seen together", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size("a"))

  expect_equal(res$id, "a")
  expect_equal(res$size, 1)               # cohort size excludes the focal cub
  expect_equal(res$survive.to.grad, 1)
  expect_setequal(cohort_members(res, "a"), c("a", "b"))
})

test_that("cubs whose den period overlaps < 120 days are excluded", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size("a"))

  expect_false("c" %in% cohort_members(res, "a"))  # c overlaps a only ~62 days
})

test_that("cubs never seen in a session with the focal cub are excluded", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size("a"))

  expect_false("d" %in% cohort_members(res, "a"))  # d overlaps but shares no session
})

test_that("cubs from a different birth clan are excluded", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size("a"))

  expect_false("e" %in% cohort_members(res, "a"))  # e is serena.n, not talek
})

test_that("a cub that died before graduation has survive.to.grad 0", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size("f"))

  expect_equal(res$survive.to.grad, 0)
  expect_equal(res$size, 0)
})

test_that("survive.to.grad is matched to the correct id for multiple cubs", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size(c("a", "f")))

  expect_equal(res$survive.to.grad[res$id == "a"], 1)
  expect_equal(res$survive.to.grad[res$id == "f"], 0)
})

test_that("ids absent from tblLifeHistory.wide warn and are dropped", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- NULL
  w <- capture_warnings(res <- calculate_cohort_size(c("a", "ghost")))

  expect_true(any(grepl("not in tblLifeHistory", w)))
  expect_equal(res$id, "a")
})

test_that("all-unknown ids return an empty result without error", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- suppressWarnings(calculate_cohort_size("ghost"))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_setequal(names(res), c("id", "size", "survive.to.grad", "cohort"))
})

test_that("a requested cub with no dob returns an NA row without error", {
  use_cohort_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  # g has no dob; it is excluded but should still be reported (as NA), not error
  res <- suppressWarnings(calculate_cohort_size("g"))

  expect_equal(res$id, "g")
  expect_true(is.na(res$size))
  expect_true(is.na(res$survive.to.grad))
})
