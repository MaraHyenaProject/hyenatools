#' Get all maternal ancestors of a hyena
#'
#' Returns a character vector of a hyena's maternal ancestors, found by
#' recursively following the `mom` column of tblHyenas. The vector starts with
#' the hyena itself and ends with its most distant known maternal ancestor (the
#' founding matriarch of its matriline). This function uses tblHyenas.
#'
#' @param id A string specifying the hyena id.
#'
#' @return A character vector ordered from the hyena (first element) to its most
#'   distant known maternal ancestor (last element). If the hyena has no recorded
#'   mom, a length-one vector containing just `id` is returned.
#'
#' @examples
#' ## the maternal line of knot, from knot up to the founding matriarch
#' get_maternal_ancestors('knot')
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
