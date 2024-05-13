#' Get maternal relatedness between two individuals
#'
#' Returns a value for expected relatedness between two individuals based on maternal pedigree. 
#' It assumes all siblingships are half siblings (i.e. sibs = 0.25)
#' Returns NA if individuals are unrelated
#'
#' @param id1 A string specifying the hyena id for first individual
#' @param id2 A string specifying the hyena id for second individual
#'
#' @examples
#'
#' @export
#' 
#' 
# 
get_maternal_relatedness <- function(id1, id2){

  l1 <- get_maternal_ancestors(id1)
  l2 <- get_maternal_ancestors(id2)

  shared_ancestors <- intersect(l1,l2)

  if(length(shared_ancestors)){
    most_recent_ancestor <- shared_ancestors[1]
    l1_line <- l1[1:which(l1 == most_recent_ancestor)]
    l2_line <- l2[1:which(l2 == most_recent_ancestor)]

    maternal_steps <- (length(l1_line)-1) + (length(l2_line)-1)
    return(1/(2^maternal_steps))
  }else{
    return(NA)
  }
}