#' Is a hyena still alive?
#'
#' Determines whether each supplied hyena is still alive, based on whether it has
#' a recorded disappearance date in tblLifeHistory.wide. A hyena with no
#' disappearance date is treated as still alive; one with a disappearance date is
#' treated as no longer alive (dead or emigrated). This function uses
#' tblLifeHistory.wide.
#'
#' @param ids One or more hyena ids.
#'
#' @return A logical vector the same length as `ids`: TRUE if the hyena is still
#'   alive (no recorded disappearance date), FALSE if it has disappeared, and NA
#'   for ids not found in tblLifeHistory.wide.
#'
#' @examples
#' is_alive('kb')
#' is_alive(c('kb', 'mrph'))
#'
#' @export

is_alive <- function(ids){
  ##############################################################################
  ### Error checking ###
  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs))
    warning('hyenadata package not loaded. This function may not work as expected.')

  if(!exists('tblLifeHistory.wide')){
    data("tblLifeHistory.wide")
    warning('tblLifeHistory.wide not in environment. Loading tblLifeHistory.wide from hyenadata package')
  }

  ids <- tolower(as.character(ids))

  idx <- match(ids, tblLifeHistory.wide$id)
  if(any(is.na(idx))){
    warning(paste(ids[is.na(idx)], collapse = ', '),
            ' not in tblLifeHistory.wide. Returning NA.')
  }

  ##############################################################################
  # Alive = no recorded disappearance date
  alive <- is.na(tblLifeHistory.wide$disappeared[idx])
  # Unknown for any id we couldn't find
  alive[is.na(idx)] <- NA

  return(alive)
}
