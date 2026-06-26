# hyenatools tests

## Running the tests

From the package root:

```sh
Rscript tests/run_tests.R
```

or, interactively:

```r
testthat::test_dir("tests/testthat")
```

## How the tests are set up

These tests deliberately do **not** use `devtools::load_all()` /
`devtools::test()` / `R CMD check`. Two work-in-progress files currently contain
invalid R syntax and prevent the package from parsing as a whole:

- `R/get_ars.R` — `get_annual_reproductive_success` is missing `<- function` and
  has an unfinished body.
- `R/get_maternal_descendanys.R` — the `get_maternal_descendants` function is
  valid, but line 28 is a leftover scratch note with broken syntax.

Until those are finished, `tests/testthat/helper-hyenatools.R` sources every
`R/*.R` file that parses (and skips, with a message, the ones that don't). When
the WIP files are fixed they will be picked up automatically, and this whole
setup can be replaced with a conventional `tests/testthat.R` +
`testthat::test_check("hyenatools")`.

## Test data

hyenatools functions read their tables (`tblHyenas`, `tblClanMembership`, ...)
as global variables loaded from the private `hyenadata` package. To keep tests
fast, deterministic, and independent of the real data, each test installs small
hand-built fixtures into the global environment via the `use_*_fixtures()`
helpers in `helper-fixtures.R`. These shadow the real tables, so known inputs
let us assert exact expected outputs.

Note: `is_descendant()` checks `sessionInfo()$otherPkgs` and stops unless the
`hyenadata` package is *attached*, so its tests `skip_if_not_installed()` and
attach hyenadata; the data still comes from the fixture.

## Coverage and known bugs

Behavioural tests cover: `get_maternal_ancestors`, `get_maternal_relatedness`,
`is_descendant`, `annual_clan_membership`, and `get_clan_status`.

Two functions have tests that **document existing bugs** (they assert the current
broken behaviour, so fixing the bug will turn the test red and prompt an update):

- `annual_clan_membership()` errors ("differing number of rows") for a year with
  no matching members, instead of returning an empty result.
- `get_monthly_clan_membership()` references an undefined variable `y` in its main
  loop (should be the per-iteration year, as in `annual_clan_membership()`).

Not yet covered: `calculate_cohort_size` and `get_association_network` (both need
larger multi-table fixtures and, for the latter, `asnipe`/`igraph` plotting).
