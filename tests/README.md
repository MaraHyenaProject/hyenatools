# hyenatools tests

## Running the tests

During development, from the package root:

```sh
Rscript tests/run_tests.R
```

or, interactively:

```r
devtools::test()
```

`R CMD check` runs the suite via `tests/testthat.R`
(`testthat::test_check("hyenatools")`) against the installed package.

## How the tests are set up

This is a standard testthat (edition 3) setup. `devtools::test()` loads the
package with `load_all()` and runs the files in `tests/testthat/`; no install is
required for local development. (Earlier the suite had to source the `R/*.R`
files directly because two work-in-progress files did not parse -- that is no
longer necessary now that they are fixed.)

## Test data

hyenatools functions read their tables (`tblHyenas`, `tblClanMembership`,
`tblLifeHistory.wide`, ...) as global variables loaded from the private
`hyenadata` package, rather than taking them as arguments. To keep tests fast,
deterministic, and independent of the real data, each test installs small
hand-built fixtures into the global environment via the `use_*_fixtures()`
helpers in `helper-fixtures.R`. These shadow the real tables, so known inputs let
us assert exact expected outputs.

Note: `is_descendant()` and `get_annual_reproductive_success()` check
`sessionInfo()$otherPkgs` and warn (or, for `is_descendant`, stop) unless the
`hyenadata` package is *attached*, so their tests `skip_if_not_installed()` and
attach hyenadata; the data still comes from the fixtures.

## Coverage

Behavioural tests cover:

- `get_maternal_ancestors`
- `get_maternal_descendants`
- `get_maternal_relatedness`
- `is_descendant`
- `get_clan_status`
- `get_annual_reproductive_success`

Not yet covered: `calculate_cohort_size` and `get_association_network` (both need
larger multi-table fixtures and, for the latter, `asnipe`/`igraph` plotting).
