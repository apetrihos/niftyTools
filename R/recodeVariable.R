#' Recode a variable
#'
#' Takes a variable and returns a numeric recoded version
#' from 1-n unique values
#'
#' @param variable A variable to be recoded 1-n
#' @return A recoded variable
#' @export

recode_variable <- function(variable, presort = TRUE) {

  unique_values <- as.character(na.omit(unique(variable)))

  if (presort) unique_values %<>%
    naturalsort::naturalsort()

  tryCatch(
    {

      recodeString <- paste0("'", unique_values, "'", "=", seq_along(unique_values),
                             collapse = ";")

      new_var <- recodeString %>%
        car::recode(as.character(variable), ., ) %>%
        as.numeric()

      levels(new_var) <- trimws(unique_values)

      attr(new_var, 'recodeString') <- recodeString

      logger::log_info("Successfully recoded variable")
      return(new_var)
    },
    error = function(e) {
      logger::log_error("Could not recode variable")
      break
    }
  )
}
