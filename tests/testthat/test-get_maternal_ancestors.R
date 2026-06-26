test_that("returns the id itself for a hyena with no recorded mom", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(get_maternal_ancestors("mm"), "mm")
})

test_that("returns the full maternal line from id to the founding matriarch", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(get_maternal_ancestors("cc"), c("cc", "bb", "aa", "mm"))
  expect_equal(get_maternal_ancestors("aa"), c("aa", "mm"))
})

test_that("an unrelated line resolves to its own founder", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(get_maternal_ancestors("yy"), c("yy", "zz"))
})
