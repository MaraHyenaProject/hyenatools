#' Identify living hyenas in specified clan on an annual basis
#'
#' Identifies all hyenas alive in a specified year for the given clan. This count
#' includes any individuals who were considered immigrants or residents and who
#' were alive and in the clan for at least one day during the year.
#'
#' @param clan One (or more) clan names, supplied as a character vector.
#' @param years One (or more) year, supplied as a numeric vector
#'
#' @return Returns tblClanMembership filtered to all individuals who were a member
#' of the supplied clan at some point in the supplied year. Columns include the
#' start and end date of membership in the clan.
#'
#' @examples
#' ## returns all hyenas from serena clans 2010-2012
#' annual_clan_membership(clans = c('happy.zebra', 'serena.s', 'serena.n'), years = 2010:2012)
#'
#' @export
#'

annual_clan_membership <- function(clans, years){
  ##############################################################################
  ### Error checking ###
  if(any(is.na(years)))
    warning('years contains NAs')
  if(any(!as.numeric(years))){
    error('Please provide years as numbers')
  }

  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs))
    warning('hyenadata package not loaded. This function may not work as expected.')

  if(!exists('tblClanMembership')){
    data(tblClanMembership)
    warning('tblClanMembership not in environment. Loading tblClanMembership from hyenadata package')
  }

  ##############################################################################
  tcm <- tblClanMembership
  tcm$start_year <- as.numeric(format(tcm$start_date, '%Y'))
  tcm$end_year <- as.numeric(format(tcm$end_date, '%Y'))

  df <- data.frame()
  for(y in years){
    n.df <- cbind(data.frame(year = y),
                  tcm[!is.na(tcm$clan) & tcm$clan %in% clans &
                        !is.na(tcm$start_year) & tcm$start_year <= y &
                        (is.na(tcm$end_year) | tcm$end_year >= y),names(tblClanMembership)])

    ### Remove notes about fission/dispersal if they are from prior years
    n.df[n.df$notes %in% c('dispersal', 'fission') & format(n.df$start_date, '%Y') != as.character(y),'notes'] <- ''
    df <- rbind(df, n.df)
  }
  if(nrow(df))
    df <- dplyr::arrange(df, clan, year, status, id)
  return(df)
}
