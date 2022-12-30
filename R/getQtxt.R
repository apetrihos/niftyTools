#' Pulls Attribute info
#'
#' This function is a helper that pulls attribute information
#'
#' @param var A single variable with attributes
#' @param attr A vector of attributes to pull
#' @return The info contained in the attribute/s
#' @export

get_info <- function(var, attr = c("questionText", "generalInfo")) {
  assertive::assert_is_non_empty(attr)

  logger::log_info(glue::glue('Pulling attribute data for ',
                              '{paste0(attr, collapse = " AND ")}'))

  tmp <- purrr::map(attr, function(attribute_name) {

    attr(var, which = attribute_name, exact = TRUE)

  }) %>%
    purrr::set_names(attr) %>%
    .[!sapply(., is.null)]

  if (length(tmp) != length(attr)) logger::log_warn(
    glue::glue('Could not find attribute info for ',
               '{paste0(attr[!attr %in% names(tmp)], collapse = " OR ")}')
  )

  return(tmp)

}
