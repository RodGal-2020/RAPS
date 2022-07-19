#' Simulate the evolution of a given P system
#'
#' TODO: The description.
#' @param rap A rap object.
#' @param n_steps The number of evolutive steps to simulate.
#' @return A rap object.
#' @examples
#' TODO.
#' @section Warning:
#' This function comes with no warranty.
#' @export
simulate = function(rap, n_steps) {
  cat("\nSimulating", crayon::bold(n_steps), "steps")

  for (i in 1:n_steps) {
    chosen_rule_info = choose_rule(rap)
    rule_id = chosen_rule_info[1]
    membrane_id = chosen_rule_info[2]

    rap %<>% apply_rule(rule_id, membrane_id)
  }

  return(rap)
}
