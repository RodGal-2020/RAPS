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

  # rap$Rules # Main section of the rap object to take into account

  return(rap)
}
