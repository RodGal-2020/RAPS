#' Apply a rule to a given rap object
#'
#' @param rap A rap object-
#' @param rule_id The id of the rule to be applied-
#' @return A new rap object, the result of applying the given rule-
#' @examples
#' TODO
#' @seealso
#' `apply_rules` to apply more than one rule at the same time.
#' @export
apply_rule = function(rap, rule_id) {

  cat("\n\tApplying the rule with id", crayon::italic(rule_id), "to the system")

  ##############################
  # Application
  ##############################
  # rap$Rules # Main section of the rap object to take into account
  rule_info = rap$Rules[rule_id, ]

  rule_info$lhs[[1]] # Demo: a, b*2
  colnames(rule_info$lhs[[1]]) = c("object", "rule_multiplicity")

  # Case: w/o dissolution
  if (rule_info$dissolves) {
    cat("\nDissolution is yet to be implemented")
  } else {

    ########################
    ###### Remove LHS ######
    ########################
    affected_lhs_membranes = rap$RAP %>%
      dplyr::filter(label == rule_info$lhs_membrane_label)

    considered_objects = affected_lhs_membranes %$%
      objects # c*3, d*4; e*5, f*6

    for (i in 1:length(considered_objects)) {
      affected_lhs_membranes$objects[[i]] %<>%
        dplyr::left_join(rule_info$lhs[[1]]) %>% # To preserve previous objects
        dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
        dplyr::select(object, multiplicity)
    }

    ##########################
    ###### Update rap ########
    rap$RAP %<>%
      dplyr::filter(label != rule_info$lhs_membrane_label) %>% # Untouched ones
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
      # affected_rhs_membranes$objects[[i]] %<>%
      affected_rhs_membranes$objects[[i]] %>%
        dplyr::left_join(rule_info$lhs[[1]]) %>%
        dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
        dplyr::select(object, multiplicity)
    }

    ##########################
    ###### Update rap ########
    rap$RAP %<>%
      dplyr::filter((label != rule_info$lhs_membrane_label) & (label != rule_info$rhs_membrane_label)) %>% # Untouched ones
      dplyr::bind_rows(affected_lhs_membranes) %>%
      dplyr::bind_rows(affected_rhs_membranes) %>%
      dplyr::arrange(label) # id could also work


  }
  # Notes
  rule_info$rhs # Demo: a, b*2
  rule_info$lhs_membrane_label # Demo: 1
  rule_info$rhs_membrane_label # Demo: 1

  affected_membranes = rap$RAP %>%
    dplyr::filter(label == rule_info$lhs_membrane_label)

  if (rule_info$dissolves) {
    # rap$RAP %<>%
    #   dplyr::filter(label != rule_info$rhs_membrane_label) # We delete all the rhs membranes
  }

  return(rap)
}
