#' Template for new functions
#'
#' @description
#' This is a template.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param my_param Yep, it's a parameter.
#'
#' @return
#' It returns...
#'
#' @examples
#' Some examples
#'
#' @section Warning:
#' This is a warning
#'
#' @export
get_concentration_of = function(rap, membrane_id, chosen_object, is_coded = FALSE) {
  labels = rap$Configuration$label
  ids = rap$Configuration$id
  coded_id = ids[which(ids == membrane_id)]

  if (is_coded) {
    return(rap$Configuration %>%
             dplyr::filter(id == coded_id) %$%
             objects %>%
             magrittr::extract2(1) %>%
             dplyr::filter(object == chosen_object) %$%
             multiplicity %>%
             sum(0))
  } else {
    return(rap$Configuration %>%
             dplyr::filter(id == membrane_id) %$% # This should work if we use the recodification
             objects %>%
             magrittr::extract2(1) %>%
             dplyr::filter(object == chosen_object) %$%
             multiplicity %>%
             sum(0))
  }
}
