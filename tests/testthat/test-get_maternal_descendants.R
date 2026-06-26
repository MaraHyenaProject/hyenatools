# Pedigree fixture (see helper-fixtures.R):
#        mm                 zz
#       /  \                 |
#      aa   dd               yy
#      |
#      bb
#      |
#      cc

test_that("returns NULL for a hyena with no offspring", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  expect_null(get_maternal_descendants("cc"))
  expect_null(get_maternal_descendants("yy"))
})

test_that("returns direct offspring at generation 1", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_maternal_descendants("zz")

  expect_s3_class(res, "data.frame")
  expect_equal(res$id, "yy")
  expect_equal(res$generation_step, 1)
  expect_setequal(names(res), c("id", "birthdate", "mom", "generation_step"))
})

test_that("collects all descendants across generations with correct depths", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_maternal_descendants("mm")

  expect_setequal(res$id, c("aa", "dd", "bb", "cc"))

  gen <- setNames(res$generation_step, res$id)
  expect_equal(gen[["aa"]], 1)  # direct offspring
  expect_equal(gen[["dd"]], 1)  # direct offspring
  expect_equal(gen[["bb"]], 2)  # grand-offspring
  expect_equal(gen[["cc"]], 3)  # great-grand-offspring
})

test_that("the matriarch is not included in her own descendants", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_maternal_descendants("mm")

  expect_false("mm" %in% res$id)
})

test_that("a mid-line ancestor returns only its own descendants", {
  use_pedigree_fixtures()
  on.exit(clear_fixtures(), add = TRUE)

  res <- get_maternal_descendants("aa")

  expect_setequal(res$id, c("bb", "cc"))
  gen <- setNames(res$generation_step, res$id)
  expect_equal(gen[["bb"]], 1)
  expect_equal(gen[["cc"]], 2)
})
