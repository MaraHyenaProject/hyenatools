#' Calculate den cohort size experienced by each cub
#'
#' Calculates a cohort size (and cohort members) experienced by the supplied cub.
#' Cohorts are specific to each cub. To be included in a cohort, cubs must have a den
#' period that overlaps with the focal cub by at least 4 months and must be seen
#' with focal cub at least once while they are both den dependent. Den period is
#' calculated as (DOB to DenGrad), (DOB to 1yr old) if no DenGrad specified, or
#' (DOB to Disappeared) if the cub died before graduation or 1 year old.
#'
#' @param List of ids to calculate cohort size for
#'
#' @return A data frame with the ids supplied, size of cohort, whether the id
#' survived to graduation (1 = yes, 0 = no) and the list of cubs in the cohort.
#'
#' @export

calculate_cohort_size <- function(ids){
  if(!exists('tblHyenas')){
    data("tblHyenas")
  }
  if(!exists('tblLifeHistory')){
    data("tblLifeHistory")
  }
  if(!exists('tblLifeHistory.long')){
    data("tblLifeHistory.long")
  }
  if(!exists('tblHyenasPerSession')){
    data("tblHyenasPerSession")
  }
  if(!exists('tblSessions')){
    data("tblSessions")
  }


  ids <- tolower(as.character(ids))
  ids.full <- ids
  missing <- which(!ids %in% tblLifeHistory.long$id)
  if(length(missing)){
    warning('Some hyenas not in tblLifeHistory! Removing:\n', paste(ids[missing],
                                                                    collapse = ','))
    ids <- ids[-missing]
  }

  if(!'date' %in% names(tblHyenasPerSession)){
    tblhps.tmp <- dplyr::left_join(tblHyenasPerSession, tblSessions[,c('session', 'date')],
                                   by = 'session')
  }else{tblhps.tmp <- tblHyenasPerSession}


  cubs.of.interest <- dplyr::filter(dplyr::left_join(tblHyenas, tblLifeHistory.long, by = 'id'), id %in% ids)

  if(any(is.na(cubs.of.interest$DOB))){
    warning(paste0('Some ids provided have no DOB. Excluding:\n',
                   paste(dplyr::filter(cubs.of.interest, is.na(DOB))$id, collapse = ',')))
    cubs.of.interest <- dplyr::filter(cubs.of.interest, !is.na(DOB))
  }

  cubs.of.interest$clan <- dplyr::left_join(cubs.of.interest, dplyr::filter(tblLifeHistory, event_code == 'DOB'), by = 'id')$event_data

  cohortInfo <- dplyr::filter(dplyr::left_join(tblHyenas, tblLifeHistory.long, by = 'id'), !is.na(DOB))
  cohortInfo$clan <-  dplyr::left_join(cohortInfo, dplyr::filter(tblLifeHistory, event_code == 'DOB'), by = 'id')$event_data
  cohortInfo <- dplyr::filter(cohortInfo, clan %in% cubs.of.interest$clan)
  cohortInfo$DenEnd <- NA
  cohortInfo$Survive_to_Grad <- 1
  for(row in 1:nrow(cohortInfo)){
    if(is.na(cohortInfo[row,'DenGrad'])){
      if(is.na(cohortInfo[row,'Disappeared'])){
        cohortInfo[row,'DenEnd'] <- cohortInfo[row,'DOB']+365
      }else{
        if(cohortInfo[row,]$DOB+365 < cohortInfo[row,]$Disappeared){
          cohortInfo[row,'DenEnd'] <- cohortInfo[row,]$DOB+365
        }else{
          cohortInfo[row,'DenEnd'] <- cohortInfo[row,]$Disappeared
          cohortInfo[row,]$Survive_to_Grad <- 0
        }
      }
    }else{cohortInfo[row,'DenEnd'] <- min(as.numeric(cohortInfo[row,'DenGrad']),  as.numeric(cohortInfo[row,'DOB']+365), na.rm = T) }
  }

  calc_overlap <- function(cub1, cub2){
    return(
      length(
        dplyr::intersect(
          seq(as.numeric(cohortInfo[cohortInfo$id == cub1,'DOB']), cohortInfo[cohortInfo$id == cub1, 'DenEnd']),
          seq(as.numeric(cohortInfo[cohortInfo$id == cub2,'DOB']), cohortInfo[cohortInfo$id == cub2, 'DenEnd'])
        )
      )
    )
  }

  seen_together <- function(cub1, cub2){
    return(
      length(
        dplyr::intersect(
          dplyr::filter(tblhps.tmp, id == cub1,
                        date >= cohortInfo[cohortInfo$id == cub1,'DOB'],
                        date <= cohortInfo[cohortInfo$id == cub1,'DenEnd'])$session,
          dplyr::filter(tblhps.tmp, id == cub2,
                        date >= cohortInfo[cohortInfo$id == cub2,'DOB'],
                        date <= cohortInfo[cohortInfo$id == cub2,'DenEnd'])$session
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
