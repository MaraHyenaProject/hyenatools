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

# --- Annual reproductive success fixture -------------------------------------
# One focal female (f1) with three cubs, plus a male (m1) and a female that died
# before breeding age (f2):
#
#   f1: female, born 2000-01-01, disappeared 2010-01-01
#       - c1 born 2003-06-01, disappeared 2006-06-01  (lived ~3y -> survived to 2)
#       - c2 born 2003-09-01, disappeared 2004-01-01  (lived ~4m -> did NOT)
#       - c3 born 2005-01-01, disappeared NA          (no end date -> survived to 2)
#   m1: male,   born 2000-01-01                        (not female; 0 offspring)
#   f2: female, born 2008-01-01, disappeared 2008-06-01 (died before age 2)
#   f3: female, born 2015-01-01, disappeared NA         (still alive)
#       - c4 born 2017-01-01, disappeared NA            (no end date -> survived to 2)
use_ars_fixtures <- function(envir = globalenv()) {
  tblHyenas <- data.frame(
    id        = c("f1", "m1", "f2", "c1", "c2", "c3", "f3", "c4"),
    mom       = c(NA,   NA,   NA,   "f1", "f1", "f1", NA,   "f3"),
    sex       = c("f",  "m",  "f",  "u",  "u",  "u",  "f",  "u"),
    birthdate = as.Date(c("2000-01-01", "2000-01-01", "2008-01-01",
                          "2003-06-01", "2003-09-01", "2005-01-01",
                          "2015-01-01", "2017-01-01")),
    stringsAsFactors = FALSE
  )
  tblLifeHistory.wide <- data.frame(
    id          = c("f1", "m1", "f2", "c1", "c2", "c3", "f3", "c4"),
    disappeared = as.Date(c("2010-01-01", NA, "2008-06-01",
                            "2006-06-01", "2004-01-01", NA, NA, NA)),
    stringsAsFactors = FALSE
  )
  assign("tblHyenas", tblHyenas, envir = envir)
  assign("tblLifeHistory.wide", tblLifeHistory.wide, envir = envir)
  invisible(tblHyenas)
}

# --- Association-network fixture ---------------------------------------------
# Sessions and who was seen in each, designed so the resulting co-occurrence
# networks are predictable. An edge exists between two hyenas iff they share a
# session.
#
#   talek    s1 (2010-01-10): a, b
#   talek    s2 (2010-01-20): b, c
#   serena.n s3 (2010-01-15): d, e
#   talek    s4 (2010-03-10): a, c
#   talek    s5 (2010-03-20): a, c
#   serena.n s6 (2010-02-15): d, e
#
# Each non-empty window has >= 2 sessions on purpose: asnipe::get_network()
# errors on a single-session (one-group) window, so we avoid that here.
# A one-month-windowed talek run yields: Jan network {a,b,c} with edges a-b and
# b-c; an empty February (skipped); a March network {a,c} with edge a-c.
# tblHyenasPerSession deliberately has no `clan` column, so the function derives
# it by joining tblSessions (the intended, supported path).
use_network_fixtures <- function(envir = globalenv()) {
  tblSessions <- data.frame(
    session = c("s1", "s2", "s3", "s4", "s5", "s6"),
    clan    = c("talek", "talek", "serena.n", "talek", "talek", "serena.n"),
    date    = as.Date(c("2010-01-10", "2010-01-20", "2010-01-15",
                        "2010-03-10", "2010-03-20", "2010-02-15")),
    stringsAsFactors = FALSE
  )
  tblHyenasPerSession <- data.frame(
    id      = c("a", "b", "b", "c", "d", "e", "a", "c", "a", "c", "d", "e"),
    session = c("s1", "s1", "s2", "s2", "s3", "s3", "s4", "s4", "s5", "s5", "s6", "s6"),
    date    = as.Date(c("2010-01-10", "2010-01-10", "2010-01-20", "2010-01-20",
                        "2010-01-15", "2010-01-15", "2010-03-10", "2010-03-10",
                        "2010-03-20", "2010-03-20", "2010-02-15", "2010-02-15")),
    stringsAsFactors = FALSE
  )
  # Used by get_clan_status() for vertex colouring
  tblClanMembership <- data.frame(
    id         = c("a", "b", "c", "d", "e"),
    clan       = c("talek", "talek", "talek", "serena.n", "serena.n"),
    start_date = as.Date(rep("2009-01-01", 5)),
    end_date   = as.Date(rep(NA, 5)),
    status     = rep("resident", 5),
    notes      = rep("", 5),
    stringsAsFactors = FALSE
  )
  assign("tblSessions", tblSessions, envir = envir)
  assign("tblHyenasPerSession", tblHyenasPerSession, envir = envir)
  assign("tblClanMembership", tblClanMembership, envir = envir)
  invisible(tblSessions)
}

# --- Cohort-size fixture -----------------------------------------------------
# Den period runs dob..denend; with no dengrad/disappeared, denend = dob + 365.
# A cub joins the focal cub's cohort if their den periods overlap >= 120 days AND
# they were seen together in >= 1 session during that period, and they share a
# birth clan (dob_event_data).
#
#   a (focal): talek, dob 2010-01-01, alive   -> den 2010-01-01 .. 2011-01-01
#   b:         talek, dob 2010-03-01, alive   -> overlaps a ~307d; seen with a (ss1)
#   c:         talek, dob 2010-11-01, alive   -> overlaps a only ~62d (< 120): OUT
#   d:         talek, dob 2010-02-01, alive   -> overlaps a ~335d but NEVER seen
#                                                with a (ss2): OUT
#   e:         serena.n, dob 2010-01-15       -> different clan: OUT
#   f:         talek, dob 2010-01-01, disappeared 2010-06-01 (died < 1y; grad = 0)
#                                                seen only in ss3, with nobody else
#   g:         no dob recorded (used to test the no-eligible-cubs path)
#
# So a's cohort = {a, b} (size 1); f's cohort = {f} (size 0, survive.to.grad 0).
use_cohort_fixtures <- function(envir = globalenv()) {
  tblHyenas <- data.frame(id = c("a", "b", "c", "d", "e", "f", "g"),
                          stringsAsFactors = FALSE)
  tblLifeHistory.wide <- data.frame(
    id             = c("a", "b", "c", "d", "e", "f", "g"),
    dob            = as.Date(c("2010-01-01", "2010-03-01", "2010-11-01",
                               "2010-02-01", "2010-01-15", "2010-01-01", NA)),
    dob_event_data = c("talek", "talek", "talek", "talek", "serena.n", "talek", NA),
    dengrad        = as.Date(rep(NA, 7)),
    disappeared    = as.Date(c(NA, NA, NA, NA, NA, "2010-06-01", NA)),
    stringsAsFactors = FALSE
  )
  tblHyenasPerSession <- data.frame(
    id      = c("a", "b", "d", "f"),
    session = c("ss1", "ss1", "ss2", "ss3"),
    date    = as.Date(c("2010-06-01", "2010-06-01", "2010-06-15", "2010-04-01")),
    stringsAsFactors = FALSE
  )
  tblSessions <- data.frame(
    session = c("ss1", "ss2", "ss3"),
    date    = as.Date(c("2010-06-01", "2010-06-15", "2010-04-01")),
    stringsAsFactors = FALSE
  )
  assign("tblHyenas", tblHyenas, envir = envir)
  assign("tblLifeHistory.wide", tblLifeHistory.wide, envir = envir)
  assign("tblHyenasPerSession", tblHyenasPerSession, envir = envir)
  assign("tblSessions", tblSessions, envir = envir)
  invisible(tblHyenas)
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
