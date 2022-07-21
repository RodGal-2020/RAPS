#' Apply a rule to a given rap object
#'
#' `r lifecycle::badge("experimental")`
#' @param rap A rap object-
#' @param rule_id The id of the rule to be applied-
#' @return A new rap object, the result of applying the given rule-
#' @examples
#' TODO
#' @seealso
#' `apply_rules` to apply more than one rule at the same time.
#' #' @section TODO:
#' * Print the trace of the execution, perhaps with the `RAPS::show_rap()` function.
#' @export
apply_rule = function(rap, rule_id) {

  cat("\n\tApplying the rule with id", crayon::bold(rule_id))

  ##############################
  # Application
  ##############################
  rule_info = rap$Rules[rule_id, ]

  # NOTES:
  # rule_info$lhs[[1]] # Demo: a, b*2
  # rap$RAP %>%
  #   dplyr::filter(label == rule_info$lhs_membrane_label) %$%
  #   objects # a*1, b*2; c*3, d*4

  # To directly avoid duplicates with dplyr
  colnames(rule_info$lhs[[1]]) = c("object", "rule_multiplicity")
  colnames(rule_info$rhs[[1]]) = c("object", "rule_multiplicity")

  # Case w/ dissolution
  if (rule_info$dissolves) {
    cat("\nDissolution is yet to be implemented")
    # rap$RAP %<>%
    #   dplyr::filter(label != rule_info$rhs_membrane_label) # We delete all the rhs membranes

  # Case w/o dissolution
  } else {

    ########################
    ###### Remove LHS ######
    ########################
    affected_lhs_membranes = rap$RAP %>%
      dplyr::filter(label == rule_info$lhs_membrane_label)

    considered_objects = affected_lhs_membranes %$%
      objects # a*1, b*2; c*3, d*4

    for (i in 1:length(considered_objects)) {
      affected_lhs_membranes$objects[[i]] %<>%
        dplyr::left_join(rule_info$lhs[[1]], by = "object") %>% # To preserve previous objects
        dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
        dplyr::select(object, multiplicity)
    }

    ##########################
    ###### Update rap ########
    rap$RAP %<>%
      dplyr::filter(label != rule_info$lhs_membrane_label) %>% # Untouched by the LHS
      dplyr::bind_rows(affected_lhs_membranes) %>%
      dplyr::arrange(label) # id could also work


    #######################
    ###### Add RHS ########
    #######################
    affected_rhs_membranes = rap$RAP %>%
      dplyr::filter(label == rule_info$rhs_membrane_label)

    considered_objects = affected_rhs_membranes %$%
      objects # c*3, d*4; e*5, f*6 modified perhaps by previous rule

    for (i in 1:length(considered_objects)) {
      affected_rhs_membranes$objects[[i]] %<>%
        dplyr::full_join(rule_info$rhs[[1]], by = "object") %>%
        dplyr::mutate(multiplicity = tidyr::replace_na(multiplicity, 0) + tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
        dplyr::select(object, multiplicity)
    }

    ##########################
    ###### Update rap ########
    rap$RAP %<>%
      dplyr::filter(label != rule_info$rhs_membrane_label) %>% # Untouched ones
      dplyr::bind_rows(affected_rhs_membranes) %>%
      dplyr::arrange(label) # id could also work
  }

  return(rap)
}
