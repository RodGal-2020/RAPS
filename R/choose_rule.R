#' Choose a rule to be applied in a rap object, following some semantics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' **Deprecation info**: This function was developed for a previous version of RAPS, with a different workflow in mind.
#'
#' Note that the rule is not applied.
#' @param rap A rap object.
#'
#' @return
#' The id of a rule which is to be applied.
#'
#' @examples
#' TODO.
#'
#' @export
choose_rule = function(rap) {
  cat(crayon::italic("\n\tchoose_rule"), "is a template, returning random rule by default")
  n_rules = dim(rap$Rules)[1]
  rule_id = rap$Rules[sample(x = 1:n_rules, size = 1)]$rule_id


  cat("\n\tWe choose the rule with id:", crayon::bold(rule_id))
  return(rule_id)
}
