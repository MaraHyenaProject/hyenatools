#' Makes all characters lowercase
#' @export
#' 
    AllCharactersToLower <- function (df) {
          # apply function to each column of dataframe
      data.frame (lapply (df, function (variable) {
          # Set the local system encoding for mac as inherited from rApache.
          # This handles multibyte error.
        Sys.setlocale('LC_ALL', locale ='C') 
          # if variable is class character, change text to lowercase
        if (is.character (variable)) return (tolower (variable)) 
        else return (variable)
      }))
    }