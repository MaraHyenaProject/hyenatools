#' Is one hyena a maternal descendant of the other?
#'
#' Returns TRUE or FALSE depending on whether the test.id is a maternal descendant of
#' the hypothesized matriarch.
#'
#' @param test.id A string specifying the name of the hypothesized descendant
#' @param matriarch A string specifying the name of the hypothesized matriarch
#'
#' @examples
#' is_descendant('knot', 'kb') # returns TRUE
#' is_descendant('pike', 'kb') # returns FALSE
#'
#' @export


is_descendant <- function(test.id, matriarch){
  #### Error checking ####
  
  #Class checking
  if(class(test.id) == 'factor'){
    test.id <- as.character(test.id)
  }
  if(class(test.id) != 'character'){
    stop('test.id must be a character')
  }

  #Check for required tables
  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs)){
    stop('hyenadata not loaded. How did you get here without doing that? Load the hyenadata package!')
  }

  if(!exists('tblHyenas')){
    data(tblHyenas)
    warning('tblHyenas not in environment. Loading tblHyenas from hyenadata package')
  }
  
  # Check length of test.id and matriarch
  if(length(test.id) > 1)
    stop('More than one test.id supplied. Please supply only one.')
  
  if(length(test.id) == 0)
    stop('No test.id supplied')
  
  if(length(test.id) > 1)
    stop('More than one matriarch supplied. Please supply only one.')
  
  if(length(test.id) == 0)
    stop('No matriarch supplied')

  if(test.id == matriarch)
    return(TRUE)

  # Make sure hyenas supplied are in the tables
  if(!test.id %in% tblHyenas$id){
    warning(paste(test.id, ' (test.id) not in tblHyenas', sep = ''))
    return(FALSE)
  }

  if(!matriarch %in% tblHyenas$id & !matriarch %in% tblHyenas$mom){
    stop(paste(matriarch, ' (matriarch) not in tblHyenas', sep = ''))
  }
  ##############################################################################

  ### check for descendants
  if(is.na(tblHyenas[tblHyenas$id == test.id,'mom']) |
     tblHyenas[tblHyenas$id == test.id,'mom'] == ''){
    return(FALSE)
  }else if(tblHyenas[tblHyenas$id == test.id,'mom'] == matriarch){
    return(TRUE)
  }else{
    return(is_descendant(tblHyenas[tblHyenas$id == test.id,'mom'], matriarch))
  }
}
