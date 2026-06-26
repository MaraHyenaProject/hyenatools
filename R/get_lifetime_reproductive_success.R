#' Calculate lifetime reproductive success
#'
#' For each focal female, returns the total number of offspring she produced that
#' survived to reproductive maturity (age 2). This function uses tblHyenas and
#' tblLifeHistory.wide.
#'
#' Offspring are attributed to a female via the `mom` column of tblHyenas, so
#' results are only meaningful for females; a warning is issued for any id that
#' is not female. An offspring counts as having survived to maturity if it lived
#' at least two years (730 days) after birth, or if it has no recorded
#' disappearance date. Note that this treats offspring with no disappearance date
#' as survivors, so lifetime reproductive success may be overestimated for
#' females with very recent offspring.
#'
#' @param ids One or more hyena ids.
#'
#' @return A data frame with one row per focal female, containing the columns
#'   `id`, `offspring_born` (total number of offspring produced), `lrs` (lifetime
#'   reproductive success: the number of those offspring that survived to age 2),
#'   and `complete_lifespan` (TRUE if the female has a recorded disappearance
#'   date, so her count covers her whole life; FALSE if she is still alive and
#'   could still reproduce; NA if her fate is unknown). See [is_alive()].
#'
#' @examples
#' ## lifetime reproductive success for kb
#' get_lifetime_reproductive_success('kb')
#'
#' @export
#'

get_lifetime_reproductive_success <- function(ids){
  ##############################################################################
  ### Error checking ###
  # Make sure tables and package exist
  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs))
    warning('hyenadata package not loaded. This function may not work as expected.')

  if(!exists('tblHyenas')){
    data("tblHyenas")
    warning('tblHyenas not in environment. Loading tblHyenas from hyenadata package')
  }
  if(!exists('tblLifeHistory.wide')){
    data("tblLifeHistory.wide")
    warning('tblLifeHistory.wide not in environment. Loading tblLifeHistory.wide from hyenadata package')
  }

  ids <- tolower(as.character(ids))

  missing <- which(!ids %in% tblHyenas$id)
  if(length(missing)){
    warning('Some hyenas not in tblHyenas! Removing:\n',
            paste(ids[missing], collapse = ','))
    ids <- ids[-missing]
  }

  non_female <- ids[!is.na(tblHyenas$sex[match(ids, tblHyenas$id)]) &
                      tblHyenas$sex[match(ids, tblHyenas$id)] != 'f']
  if(length(non_female)){
    warning('Reproductive success is calculated from the mom column, so it is ',
            'only meaningful for females. The following ids are not female and ',
            'will show 0 offspring:\n', paste(non_female, collapse = ','))
  }

  ##############################################################################
  ### Determine which offspring survived to age 2 ###
  hyenas_born <- tblHyenas[!is.na(tblHyenas$birthdate) & !is.na(tblHyenas$mom),]
  hyenas_born <- dplyr::left_join(hyenas_born,
                                  tblLifeHistory.wide[,c('id', 'disappeared')],
                                  by = 'id')

  age_at_disappearance <- difftime(hyenas_born$disappeared,
                                   hyenas_born$birthdate, units = 'days')
  # An offspring survived to 2 if it lived >= 730 days, or has no disappearance date
  hyenas_born$surv_to_2 <- 0
  hyenas_born$surv_to_2[is.na(age_at_disappearance) |
                          age_at_disappearance >= 365*2] <- 1

  ##############################################################################
  ### Tally lifetime totals for each focal female ###
  result <- data.frame(
    id = ids,
    offspring_born = rep(NA_integer_, length(ids)),
    lrs = rep(NA_integer_, length(ids)),
    stringsAsFactors = FALSE
  )
  for(i in seq_along(ids)){
    offspring <- hyenas_born[hyenas_born$mom %in% ids[i],]
    result$offspring_born[i] <- nrow(offspring)
    result$lrs[i] <- sum(offspring$surv_to_2)
  }

  # A complete lifespan means the female is no longer alive, so her count covers
  # her whole reproductive life (suppress is_alive()'s redundant table warnings)
  result$complete_lifespan <- !suppressWarnings(is_alive(ids))

  return(result)
}
