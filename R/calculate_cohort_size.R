#' Calculate den cohort size experienced by each cub
#'
#' Calculates a cohort size (and cohort members) experienced by the supplied cub.
#' Cohorts are specific to each cub. To be included in a cohort, cubs must have a den
#' period that overlaps with the focal cub by at least 4 months and must be seen
#' with focal cub at least once while they are both den dependent. Den period is
#' calculated as (dob to dengrad), (dob to 1yr old) if no dengrad specified, or
#' (dob to disappeared) if the cub died before graduation or 1 year old.
#'
#' @param List of ids to calculate cohort size for
#'
#' @return A data frame with the ids supplied, size of cohort, whether the id
#' survived to graduation (1 = yes, 0 = no) and the list of cubs in the cohort
#' (this list includes the id provided, but the cohort size does not).
#'
#' @export

calculate_cohort_size <- function(ids){
  ##############################################################################
  ### Error checking ###
  # Make sure tables and package exists
  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs))
    warning('hyenadata package not loaded. This function may not work as expected.')

  if(!exists('tblHyenas')){
    data("tblHyenas")
    warning('tblHyenas not in environment. Loading tblHyenas from hyenadata package')
  }
  if(!exists('tblLifeHistory')){
    data("tblLifeHistory")
    warning('tblLifeHistory not in environment. Loading tblLifeHistory from hyenadata package')
  }
  if(!exists('tblLifeHistory.wide')){
    data("tblLifeHistory.wide")
    warning('tblLifeHistory.wide not in environment. Loading tblLifeHistory.wide from hyenadata package')
  }
  if(!exists('tblHyenasPerSession')){
    data("tblHyenasPerSession")
    warning('tblHyenasPerSession not in environment. Loading tblHyenasPerSession from hyenadata package')
  }
  if(!exists('tblSessions')){
    data("tblSessions")
    warning('tblSessions not in environment. Loading tblSessions from hyenadata package')
  }


  ids <- tolower(as.character(ids))
  missing <- which(!ids %in% tblLifeHistory.wide$id)
  if(length(missing)){
    warning('Some hyenas not in tblLifeHistory! Removing:\n', paste(ids[missing],
                                                                    collapse = ','))
    ids <- ids[-missing]
  }
  ##############################################################################
  ids.full <- ids

  if(!'date' %in% names(tblHyenasPerSession)){
    tblhps.tmp <- dplyr::left_join(tblHyenasPerSession, tblSessions[,c('session', 'date')],
                                   by = 'session')
  }else{tblhps.tmp <- tblHyenasPerSession}


  cubs.of.interest <- dplyr::filter(dplyr::left_join(tblHyenas, tblLifeHistory.wide, by = 'id'), id %in% ids)

  if(any(is.na(cubs.of.interest$dob))){
    warning(paste0('Some ids provided have no dob. Excluding:\n',
                   paste(dplyr::filter(cubs.of.interest, is.na(dob))$id, collapse = ',')))
    cubs.of.interest <- dplyr::filter(cubs.of.interest, !is.na(dob))
  }

  cubs.of.interest$clan <- cubs.of.interest$dob_event_data

  cohortInfo <- dplyr::filter(dplyr::left_join(tblHyenas, tblLifeHistory.wide, by = 'id'), !is.na(dob))
  cohortInfo$clan <-  cohortInfo$dob_event_data
  cohortInfo <- dplyr::filter(cohortInfo, clan %in% cubs.of.interest$clan)
  cohortInfo$denend <- NA
  cohortInfo$Survive_to_Grad <- 1
  for(row in 1:nrow(cohortInfo)){
    if(is.na(cohortInfo[row,'dengrad'])){
      if(is.na(cohortInfo[row,'disappeared'])){
        cohortInfo[row,'denend'] <- cohortInfo[row,'dob']+365
      }else{
        if(cohortInfo[row,]$dob+365 < cohortInfo[row,]$disappeared){
          cohortInfo[row,'denend'] <- cohortInfo[row,]$dob+365
        }else{
          cohortInfo[row,'denend'] <- cohortInfo[row,]$disappeared
          cohortInfo[row,]$Survive_to_Grad <- 0
        }
      }
    }else{cohortInfo[row,'denend'] <- min(as.numeric(cohortInfo[row,'dengrad']),  as.numeric(cohortInfo[row,'dob']+365), na.rm = T) }
  }

  calc_overlap <- function(cub1, cub2){
    return(
      length(
        dplyr::intersect(
          seq(as.numeric(cohortInfo[cohortInfo$id == cub1,'dob']), cohortInfo[cohortInfo$id == cub1, 'denend']),
          seq(as.numeric(cohortInfo[cohortInfo$id == cub2,'dob']), cohortInfo[cohortInfo$id == cub2, 'denend'])
        )
      )
    )
  }

  seen_together <- function(cub1, cub2){
    return(
      length(
        dplyr::intersect(
          dplyr::filter(tblhps.tmp, id == cub1,
                        date >= cohortInfo[cohortInfo$id == cub1,'dob'],
                        date <= cohortInfo[cohortInfo$id == cub1,'denend'])$session,
          dplyr::filter(tblhps.tmp, id == cub2,
                        date >= cohortInfo[cohortInfo$id == cub2,'dob'],
                        date <= cohortInfo[cohortInfo$id == cub2,'denend'])$session
        )
      )
    )
  }

  cohort.size <- data.frame(id = cubs.of.interest$id,
                            size = NA,
                            survive.to.grad = cohortInfo[cohortInfo$id %in% cubs.of.interest$id,]$Survive_to_Grad,
                            cohort = NA)

  ###Assign cohort size - must overlap by at least 4 months and be seen together once
  for(cub.row in 1:nrow(cubs.of.interest)){
    ###Names of hyenas with overlapping den periods
    overlap <- names(
      which(
        sapply(
          dplyr::filter(cohortInfo, clan  == cubs.of.interest$clan[cub.row])$id,
          FUN = calc_overlap,
          cub2 = cubs.of.interest$id[cub.row]) >= 120
      )
    )
    ###Names of hyenas seen together at least once
    cohort <- names(
      which(
        sapply(
          overlap,
          FUN = seen_together,
          cub2 = cubs.of.interest$id[cub.row]) >= 1
      )
    )

    cohort.size$size[cub.row] <- max(0, length(cohort)-1)
    cohort.size$cohort[cub.row] <- paste(cohort, collapse = ',')
  }
  cohort.size$id <- as.character(cohort.size$id)
  cohort.size <- dplyr::left_join(data.frame(id = as.character(ids.full), stringsAsFactors = FALSE),
                                  cohort.size, by = 'id')

  return(cohort.size)
}
