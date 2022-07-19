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

  new_objects = rap$RAP %>%
    dplyr::filter(label == rule_info$lhs_membrane_label) %$%
    objects

  for (i in 1:new_objects) {
    new_objects[[i]] %>%
      dplyr::left_join(rule_info$lhs[[1]]) %>% # To preserve previous objects
      dplyr::mutate(rule_multiplicity = tidyr::replace_na(rule_multiplicity, 0))

    # %>%
      # dplyr::mutate(multiplicity = multiplicity - rule_multiplicity)

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
