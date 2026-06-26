# Synthetic data fixtures used by the tests.
#
# hyenatools functions read their data tables (tblHyenas, tblClanMembership, ...)
# as global variables loaded from the private `hyenadata` package, rather than
# taking them as arguments. To keep tests fast, deterministic, and independent of
# the real (private) data, each test installs small hand-built tables into the
# global environment via the use_*_fixtures() helpers below. The known structure
# lets us assert exact expected values.

# --- Pedigree fixture --------------------------------------------------------
# A small known matriline plus an unrelated line:
#
#        mm                 zz
#       /  \                 |
#      aa   dd               yy
#      |
#      bb
#      |
#      cc
#
use_pedigree_fixtures <- function(envir = globalenv()) {
  tblHyenas <- data.frame(
    id        = c("mm", "aa", "dd", "bb", "cc", "zz", "yy"),
    mom       = c(NA,   "mm", "mm", "aa", "bb", NA,   "zz"),
    birthdate = as.Date(c("2000-01-01", "2002-01-01", "2003-01-01",
                          "2004-01-01", "2006-01-01", "2000-06-01",
                          "2002-06-01")),
    stringsAsFactors = FALSE
  )
  assign("tblHyenas", tblHyenas, envir = envir)
  invisible(tblHyenas)
}

# --- Clan membership fixture -------------------------------------------------
# h1: talek, 2010-01-01 .. 2012-06-01 (natal)
# h2: talek, 2011-03-01 .. present     (resident, end_date = NA)
# h3: serena.n, 2009-05-01 .. 2010-08-01 (immigrant, dispersal note in 2009)
use_clan_fixtures <- function(envir = globalenv()) {
  tblClanMembership <- data.frame(
    id         = c("h1", "h2", "h3"),
    clan       = c("talek", "talek", "serena.n"),
    start_date = as.Date(c("2010-01-01", "2011-03-01", "2009-05-01")),
    end_date   = as.Date(c("2012-06-01", NA,           "2010-08-01")),
    status     = c("natal", "resident", "immigrant"),
    notes      = c("", "", "dispersal"),
    stringsAsFactors = FALSE
  )
  assign("tblClanMembership", tblClanMembership, envir = envir)
  invisible(tblClanMembership)
}

# Remove any fixtures we created so tests don't leak global state into each
# other or into an interactive session.
clear_fixtures <- function(names = c("tblHyenas", "tblClanMembership",
                                     "tblLifeHistory.wide", "tblHyenasPerSession",
                                     "tblSessions"),
                           envir = globalenv()) {
  for (nm in names) {
    if (exists(nm, envir = envir, inherits = FALSE)) {
      rm(list = nm, envir = envir)
    }
  }
}
