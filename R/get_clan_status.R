#' Get clan membership and status of hyenas at provided dates
#'
#' Identifies the clan that a hyena belongs to at a provided date and it's status (i.e., natal or resident)
#' at the provided date. tblClanMembership is used to determine both pieces of information.
#'
#' @param ids One or more hyena ids.
#' @param date One or more dates. If more than one date, dates must match length of ids. If one date, the date is recycled for each id.
#' @param end.of.data The date at which upper bounds of intervals with NA as final date are considered to end. You are unlikely to need to provide this.
#'
#' @return A dataframe with one row per individual in ids, including date for each individual, residence status (e.g., resident, immigrant), and clan membership. If There is no entry for an individual on the given date, NA is returned for clan and status.
#'
#' @examples
#' # returns clan membership and status for bsh on Sep 20 1989
#' get_clan_status('bsh', '1989-09-20')
#'
#' get_clan_status(c('bsh', 'mrph', 'hel'), c('1989-09-20', '2009-09-20', '2019-09-20'))
#'
#' @importFrom lubridate "%within%"
#' @export


get_clan_status <- function(ids, dates, end.of.data = '2200-01-01'){
  ##############################################################################
  ### Error checking ###
  if(any(is.na(ids)))
    warning('ids contains NAs')

  if(any(is.na(dates)))
    warning('dates contains NAs')

  if(any(dates > end.of.data)){
    warning('One ore more of the supplied dates are after the supplied end date of the data')
  }

  # Make sure tables and package exists
  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs)){
    if(exists('tblClanMembership')){
      warning('hyenadata package not loaded. This function may not work as expected.')
    }else{
      stop('hyenadata package not loaded. Load hyenadata package to proceed')
    }

  }else{
    if(!exists('tblClanMembership')){
      data(tblClanMembership)
      warning('tblClanMembership not in environment. Loading tblClanMembership from hyenadata package')
    }
  }

  if(any(!ids %in% tblClanMembership$id)){
    missing <- ids[which(!ids %in% tblClanMembership$id)]
    warning(paste0(paste(missing, collapse = ', '), ' missing from tblClanMembership. Returning NA for clan and status'))
  }


  ##############################################################################
  ids <- as.character(ids)
  df <- data.frame(ids, dates)
  df$i <- seq(1:nrow(df))
  cm <- tblClanMembership
  cm[is.na(cm$end_date),]$end_date <- end.of.data
  cm$int <- lubridate::interval(cm$start_date, cm$end_date)
  df1 <- dplyr::left_join(df, cm, by = c('ids' = 'id'))
  df1
  df1 <- df1[as.Date(df1$dates) %within% df1$int,]

  df <- dplyr::left_join(df, df1[,c('ids', 'dates', 'status', 'clan', 'i')], by = c('ids', 'dates', 'i'))
  return(df[-3])
}
