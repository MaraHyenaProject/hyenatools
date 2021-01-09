#' Load all tables
#' 
#' Loads all data tables from the hyenadata package.
#' 
#' @export
#' 

load_all_tables <- function(){
  data(list = data(package = 'hyenadata')$results[,3], package = 'hyenadata')
}