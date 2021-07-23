#' Identify living hyenas in specified clan for a month
#'
#' Identifies all hyenas alive in a specified time period for the given clan. This count
#' includes any individuals who were considered immigrants or residents and who
#' were alive and in the clan for at least one day during the month.
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

get_monthly_clan_membership <- function(clans, dates = NULL, years = NULL, months = NULL){
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
  tcm$start_month <- as.numeric(format(tcm$start_date, '%m'))
  tcm$end_month <- as.numeric(format(tcm$end_date, '%m'))
  
  if(is.null(months) & is.null(years) & !is.null(dates)){
    dates <- data.frame(month = as.numeric(format(dates, '%m')), 
                        year = as.numeric(format(dates, '%y')))
  }else if(!is.null(months) & !is.null(years) & is.null(dates)){
    if(!is.numeric(years)){
      warning('years not numeric, coercing to numeric')
    }
    if(!is.numeric(months)){
      warning('months not numeric, coercing to numeric')
    }
    dates <- data.frame(month = as.numeric(months),
                        year = as.numeric(years))
  }else if(is.null(months) & is.null(years) & is.null(dates)){
    stop('No date info provided. Please provide either dates or months/years')
  }else{
    stop('Both dates and months/years info supplied. Please supply either months/years or dates')
  }
  
  
  df <- data.frame()
  for(i in 1:nrow(dates)){
    n.df <- cbind(data.frame(year = dates$year[i]),
                  tcm[!is.na(tcm$clan) & tcm$clan %in% clans &
                        !is.na(tcm$start_year) & tcm$start_year <= dates$year[i] &
                        (is.na(tcm$end_year) | tcm$end_year >= y),names(tblClanMembership)])
    
    ### Remove notes about fission/dispersal if they are from prior years
    n.df[n.df$notes %in% c('dispersal', 'fission') & format(n.df$start_date, '%Y') != as.character(y),'notes'] <- ''
    df <- rbind(df, n.df)
  }
  if(nrow(df))
    df <- dplyr::arrange(df, clan, year, status, id)
  return(df)
}
