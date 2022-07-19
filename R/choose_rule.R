#' Choose a rule to be applied in a rap object
#'
#' Note that the rule is not applied.
#' @param rap A rap object.
#' @param semantics The path to a semantics object, or the id of a given algorithm from the following list: GIL (for Gillespie), DET (for Deterministic Waiting Times Algorithm).
#' @return The id of a rule which is to be applied and the id of the chosen membrane.
#' @examples
#' TODO.
#' @export
choose_rule = function(rap, semantics = "GIL") {
  cat(crayon::italic("\n\tchoose_rule"), "is under develpment, returning", crayon::italic("rule_id = 1, membrane_id = 1"), "by default")


  ########################################
  # Algorithm
  ########################################
  full_names = c(
    GIL = "Gillespie",
    DET = "Deterministic Waiting Times Algorithm"
  )

  if (semantics %in% names(full_names)) {
    cat("\nUsing the", crayon::bold(full_names[semantics]), "algorithm...")
  } else {
    cat("\nThe custom semantics option is not supported yet, using the Gillespie algorithm...")
    semantics = GIL
  }

  ########################################
  # Applying the algorithm
  ########################################
  rule_id = 1
  membrane_id = 1

  ########################################
  # Returning the generated exit
  ########################################
  cat("\n\tWe choose the rule with id:", crayon::bold(rule_id), "in the membrane with id:", crayon::bold(membrane_id))
  return(c(rule_id, membrane_id))
}
