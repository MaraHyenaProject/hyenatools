# is_descendant() stops unless the hyenadata package is attached (it checks
# sessionInfo()$otherPkgs), so these tests require hyenadata to be installed.
# The actual data still comes from the global-environment fixture, which shadows
# the real tables.

setup_is_descendant <- function() {
  skip_if_not_installed("hyenadata")
  suppressMessages(suppressWarnings(
    attachNamespace_ok <- require(hyenadata, quietly = TRUE)
  ))
  skip_if_not("hyenadata" %in% names(sessionInfo()$otherPkgs),
              "hyenadata not attachable")
  use_pedigree_fixtures()
}

test_that("a hyena is a descendant of its direct mom", {
  setup_is_descendant()
  on.exit(clear_fixtures(), add = TRUE)

  expect_true(is_descendant("aa", "mm"))
  expect_true(is_descendant("cc", "bb"))
})

test_that("descendancy is detected across multiple generations", {
  setup_is_descendant()
  on.exit(clear_fixtures(), add = TRUE)

  expect_true(is_descendant("cc", "mm"))
  expect_true(is_descendant("cc", "aa"))
})

test_that("a hyena counts as its own descendant (test.id == matriarch)", {
  setup_is_descendant()
  on.exit(clear_fixtures(), add = TRUE)

  expect_true(is_descendant("mm", "mm"))
})

test_that("non-descendants return FALSE", {
  setup_is_descendant()
  on.exit(clear_fixtures(), add = TRUE)

  # dd descends from mm, not from aa
  expect_false(is_descendant("dd", "aa"))
  # different matriline entirely
  expect_false(is_descendant("yy", "mm"))
  # founder has no mom
  expect_false(is_descendant("mm", "aa"))
})

test_that("an unknown test.id warns and returns FALSE", {
  setup_is_descendant()
  on.exit(clear_fixtures(), add = TRUE)

  expect_warning(result <- is_descendant("ghost", "mm"),
                 "not in tblHyenas")
  expect_false(result)
})

test_that("a non-character test.id is rejected", {
  setup_is_descendant()
  on.exit(clear_fixtures(), add = TRUE)

  expect_error(is_descendant(42, "mm"), "must be a character")
})
