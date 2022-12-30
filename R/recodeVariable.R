#' Recode a variable
#'
#' Takes a variable and returns a numeric recoded version
#' from 1-n unique values
#'
#' @param var A variable to be recoded 1-n
#' @return A recoded variable
#' @export

recodeVariable <- function(var) {
  
  uniqueValues <- unique(var)
  
  tryCatch(
    {
      newVar <- car::recode(var, paste0(na.omit(uniqueValues), '=',
                                        1:length(na.omit(uniqueValues)), collapse= ';'))
      levels(newVar) <- uniqueValues
      logger::log_info("Successfully recoded variable")
      return(newVar)
    },
    error = function(e) { 
      logger::log_error('Could not recode variable')
      break
    }
  )
}
