#' Simulate the evolution of a given P system
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
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
    chosen_rule_id = choose_rule(rap)
    rap %<>% RAPS::apply_rule(rule_id, membrane_id)
  }

  return(rap)
}
