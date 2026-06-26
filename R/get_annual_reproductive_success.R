#' Calculate annual reproductive success
#'
#' For each focal female, returns one row per year of her reproductive life
#' giving the number of offspring she produced that year and how many of those
#' offspring survived to age 2. A female's reproductive life is taken to run from
#' the year she turns 2 (the earliest she is likely to breed) through the year
#' she disappeared, or through the last year present in the data if she has no
#' disappearance date. This function uses tblHyenas and tblLifeHistory.wide.
#'
#' Offspring are attributed to a female via the `mom` column of tblHyenas, so
#' results are only meaningful for females; a warning is issued for any id that
#' is not female. A cub counts as having survived to 2 if it lived at least two
#' years (730 days) after birth, or if it has no recorded disappearance date.
#' Note that this treats cubs with no disappearance date as survivors, so
#' reproductive success for very recent years may be overestimated.
#'
#' @param ids One or more hyena ids.
#' @param end_year The last year to consider, used as the end of a female's
#'   reproductive window when she has no disappearance date. Defaults to the most
#'   recent birth year in tblHyenas (the data horizon).
#'
#' @return A data frame with one row per focal female per year, containing the
#'   columns `id`, `year`, `offspring_born`, and `offspring_surv_to_2`. Females
#'   with no observed reproductive years (e.g. those that died before age 2)
#'   contribute no rows.
#'
#' @examples
#' ## annual reproductive success for kb across her reproductive life
#' get_annual_reproductive_success('kb')
#'
#' @export
#'

get_annual_reproductive_success <- function(ids, end_year = NULL){
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
  # A cub survived to 2 if it lived >= 730 days, or has no disappearance date
  hyenas_born$surv_to_2 <- 0
  hyenas_born$surv_to_2[is.na(age_at_disappearance) |
                          age_at_disappearance >= 365*2] <- 1
  hyenas_born$birth_year <- as.numeric(format(hyenas_born$birthdate, '%Y'))

  # End of the data: the most recent year a cub could have been born
  if(is.null(end_year))
    end_year <- max(as.numeric(format(tblHyenas$birthdate, '%Y')), na.rm = TRUE)

  ##############################################################################
  ### Tally offspring per year for each focal female ###
  rows <- list()
  for(id in ids){
    birthdate <- tblHyenas[tblHyenas$id == id,]$birthdate[1]
    if(is.na(birthdate)){
      warning(id, ' has no birthdate. Skipping.')
      next
    }

    # First plausible breeding year (the year the female turns 2)
    start_year <- as.numeric(format(birthdate + 365*2, '%Y'))

    # Last year to consider: her disappearance year, capped at the data horizon
    disappeared <- tblLifeHistory.wide$disappeared[tblLifeHistory.wide$id == id]
    if(length(disappeared) && !is.na(disappeared[1])){
      last_year <- min(as.numeric(format(disappeared[1], '%Y')), end_year)
    }else{
      last_year <- end_year
    }

    # No observed reproductive years (e.g. died before age 2)
    if(start_year > last_year)
      next

    offspring <- hyenas_born[hyenas_born$mom %in% id,]
    for(y in start_year:last_year){
      cubs <- offspring[offspring$birth_year == y,]
      rows[[length(rows) + 1]] <- data.frame(
        id = id,
        year = y,
        offspring_born = nrow(cubs),
        offspring_surv_to_2 = sum(cubs$surv_to_2),
        stringsAsFactors = FALSE
      )
    }
  }

  if(length(rows) == 0){
    return(data.frame(id = character(0), year = numeric(0),
                      offspring_born = integer(0),
                      offspring_surv_to_2 = numeric(0)))
  }

  result <- do.call(rbind, rows)
  result <- dplyr::arrange(result, id, year)
  rownames(result) <- NULL
  return(result)
}
