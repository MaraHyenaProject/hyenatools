#!/usr/bin/env Rscript
#
# Data-integrity check: life-history milestones vs. sighting records
# =================================================================
#
# This is a STANDALONE utility -- it is NOT part of the hyenatools package
# (the data_checks/ folder is listed in .Rbuildignore). Run it periodically to
# flag hyenas whose life-history milestones in tblLifeHistory.wide disagree with
# where/when they actually show up in tblHyenasPerSession (dated via tblSessions).
#
# The motivating case is "zombie" hyenas: individuals with no `disappeared` date
# (so they read as still alive) who in fact stopped being seen long ago, or were
# never seen at all. `disappeared` is the authoritative life-status field, but
# accidental omissions happen, and the sighting record is the ground truth we can
# cross-check it against.
#
# Checks performed (one row per flagged hyena, with the supporting dates so a
# human can adjudicate):
#   * zombie_long_gap        : no `disappeared` date, but last seen long before
#                              the most recent session in the data (likely a
#                              missed disappearance).
#   * zombie_never_seen      : no `disappeared` date and never appears in the
#                              sighting record at all (e.g. old founders that
#                              predate tblHyenasPerSession coverage).
#   * seen_after_disappeared : appears in a session dated after `disappeared`.
#                              Large gaps (many years) usually mean an id was
#                              reused for a different individual -- check the
#                              days_after_disappeared column to triage.
#   * seen_before_dfs        : appears in a session dated before `dfs`
#                              (recorded date first seen).
#   * seen_before_dob        : appears in a session dated before `dob`.
#
# Usage:
#   Rscript data_checks/check_life_history_vs_sightings.R [gap_threshold_days] [out.csv]
# e.g.
#   Rscript data_checks/check_life_history_vs_sightings.R 365 ~/Desktop/integrity.csv
#
# Defaults: gap_threshold_days = 365, output written to data_integrity_report.csv
# in the current working directory.

suppressMessages({
  library(dplyr)
  library(hyenadata)
})

# ---------------------------------------------------------------------------
# Load the tables (use what's already in the environment if present)
# ---------------------------------------------------------------------------
load_tables <- function() {
  for (tbl in c("tblHyenas", "tblLifeHistory.wide",
                "tblHyenasPerSession", "tblSessions")) {
    if (!exists(tbl, envir = globalenv())) {
      utils::data(list = tbl, package = "hyenadata", envir = globalenv())
    }
  }
}

# ---------------------------------------------------------------------------
# Main check
# ---------------------------------------------------------------------------
check_life_history_vs_sightings <- function(gap_threshold_days = 365) {
  load_tables()

  # --- Per-hyena sighting summary from tblHyenasPerSession + tblSessions ----
  # Date comes from tblSessions (authoritative); fall back to the date already on
  # tblHyenasPerSession if a session is missing from tblSessions. We deliberately
  # do NOT use tblSessions$clan -- that column is not reliable.
  sightings <- tblHyenasPerSession %>%
    select(id, session) %>%
    left_join(select(tblSessions, session, sess_date = date),
              by = "session") %>%
    mutate(date = coalesce(sess_date,
                           tblHyenasPerSession$date[match(session,
                                                          tblHyenasPerSession$session)])) %>%
    filter(!is.na(date))

  sight_summary <- sightings %>%
    group_by(id) %>%
    summarise(
      n_sightings = n(),
      first_seen  = min(date),
      last_seen   = max(date),
      .groups     = "drop"
    )

  # --- Monitoring horizon ---------------------------------------------------
  # A single global horizon (most recent session in the data). We can't use a
  # per-clan horizon because tblSessions$clan is unreliable.
  global_last_session <- max(tblSessions$date, na.rm = TRUE)

  # --- Assemble one row per hyena ------------------------------------------
  hy <- tblHyenas %>%
    select(id, sex, birthdate) %>%
    left_join(select(tblLifeHistory.wide, id, dob, dfs, disappeared), by = "id") %>%
    left_join(sight_summary, by = "id") %>%
    mutate(
      n_sightings = coalesce(n_sightings, 0L),
      gap_days    = as.numeric(global_last_session - last_seen)  # NA when never seen
    )

  # --- Flags ----------------------------------------------------------------
  hy <- hy %>%
    mutate(
      # Alive on paper, but the sighting record says otherwise
      zombie_long_gap = is.na(disappeared) & n_sightings > 0 &
        !is.na(gap_days) & gap_days > gap_threshold_days,
      zombie_never_seen = is.na(disappeared) & n_sightings == 0,
      seen_after_disappeared = !is.na(disappeared) & !is.na(last_seen) &
        last_seen > disappeared,
      seen_before_dfs = !is.na(dfs) & !is.na(first_seen) & first_seen < dfs,
      seen_before_dob = !is.na(dob) & !is.na(first_seen) & first_seen < dob
    )

  # Human-readable issue list per hyena
  flag_cols <- c("zombie_long_gap", "zombie_never_seen",
                 "seen_after_disappeared", "seen_before_dfs", "seen_before_dob")
  hy$issues <- apply(hy[flag_cols], 1, function(r) {
    paste(flag_cols[which(r)], collapse = ", ")
  })

  flagged <- hy %>%
    filter(issues != "") %>%
    mutate(
      days_after_disappeared = ifelse(seen_after_disappeared,
                                      as.numeric(last_seen - disappeared), NA),
      days_before_dfs = ifelse(seen_before_dfs,
                               as.numeric(dfs - first_seen), NA)
    ) %>%
    select(id, sex, issues,
           birthdate, dob, dfs, disappeared,
           first_seen, last_seen, n_sightings, gap_days,
           days_after_disappeared, days_before_dfs) %>%
    arrange(desc(grepl("zombie", issues)), id)

  flagged
}

# ---------------------------------------------------------------------------
# Pretty summary
# ---------------------------------------------------------------------------
summarise_report <- function(flagged) {
  cat("\n================ Data integrity report ================\n")
  cat("Flagged hyenas:", nrow(flagged), "\n")
  for (fl in c("zombie_long_gap", "zombie_never_seen",
               "seen_after_disappeared", "seen_before_dfs", "seen_before_dob")) {
    cat(sprintf("  %-24s %d\n", fl, sum(grepl(fl, flagged$issues))))
  }
  cat("=======================================================\n\n")
}

# ---------------------------------------------------------------------------
# Run when invoked via Rscript
# ---------------------------------------------------------------------------
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  gap <- if (length(args) >= 1) as.numeric(args[[1]]) else 365
  out <- if (length(args) >= 2) args[[2]] else "data_integrity_report.csv"

  flagged <- check_life_history_vs_sightings(gap_threshold_days = gap)
  summarise_report(flagged)
  write.csv(flagged, out, row.names = FALSE)
  cat("Full report written to:", normalizePath(out, mustWork = FALSE), "\n")
}
