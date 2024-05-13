#' Get list of maternal ancestors
#'
#' Returns a vector starting with id and ending with most distant maternal ancestor. This function uses tblHyenas
#'
#' @param id A string specifying the hyena id
#'
#' @examples
#'
#' @export
#'

get_maternal_ancestors <- function(id){
  if(is.na(tblHyenas[tblHyenas$id == id, 'mom'])){
    return(id)
  }else{
    return(c(id, get_maternal_ancestors(tblHyenas[tblHyenas$id == id, 'mom'])))
  }
}
