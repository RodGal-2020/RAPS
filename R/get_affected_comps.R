#' Template for new functions
#'
#' @description
#' This is a template.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param rules A set of rules from a `rap` object.
#'
#' @return
#' It returns...
#'
#' @examples
#' print("TODO:")
#'
#' @section Warning:
#' This is a warning
#'
#' @export
get_affected_comps = function(rules) {
  ## Debugging
  # rules = rap$Rules
  n_rules = dim(rules)[1]
  affected = list()
  main_membranes = rules$main_membrane_label
  for (rule in 1:n_rules) {
    new_affected = c()
    # rule = 1
    # NOT SUITABLE FOR CRAZY MULTICOMMUNICATION (use list() instead for that case)
    w_lhs = rules$lhs[[rule]] %>%
      dplyr::filter(where != "@here" & where != "@exists") %$%
      where
    if (length(w_lhs) != 0) {
      new_affected %<>% c(w_lhs)
    }

    w_rhs = rules$rhs[[rule]] %>%
      dplyr::filter(where != "@here") %$%
      where
    if (length(w_rhs) != 0) {
      new_affected %<>% c(w_rhs)
    }

    new_affected %<>% c(main_membranes[rule])

    affected[[rule]] = unique(new_affected)
  }

  rules %<>%
    dplyr::bind_cols(tibble::tibble(affected))

  return(rules)
}
