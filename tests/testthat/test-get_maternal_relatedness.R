test_that("self-relatedness is 1", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(get_maternal_relatedness("mm", "mm"), 1)
})

test_that("mother-daughter relatedness is 0.5", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(get_maternal_relatedness("bb", "cc"), 0.5)
  expect_equal(get_maternal_relatedness("mm", "aa"), 0.5)
})

test_that("half-siblings (shared mom) are 0.25", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  # aa and dd are both offspring of mm
  expect_equal(get_maternal_relatedness("aa", "dd"), 0.25)
})

test_that("grandmother-granddaughter relatedness is 0.25", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  # mm -> aa -> bb : two maternal steps
  expect_equal(get_maternal_relatedness("mm", "bb"), 0.25)
})

test_that("relatedness is symmetric", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_equal(get_maternal_relatedness("cc", "dd"),
               get_maternal_relatedness("dd", "cc"))
})

test_that("unrelated individuals return NA", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_true(is.na(get_maternal_relatedness("cc", "yy")))
})
