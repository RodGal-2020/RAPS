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
get_rule_by_id = function(my_rap, my_rule_id) {
  return(
    my_rap$Rules %>%
      dplyr::filter(rule_id == my_rule_id)
  )
}
