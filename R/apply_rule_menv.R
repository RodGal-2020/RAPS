#' Apply a rule to a given multienvironmental P system
#'
#' `r lifecycle::badge("experimental")`
#' Apply a rule to a given multienvironmental P system given as a `rap` object.
#'
#' @param rap A rap object.
#' @param rule_id The id of the rule to be applied.
#' @param comp_id The id of the affected compartment.
#' @param verbose The verbosity, between 0 and 5.
#' @param debug Useful if you want to debug the execution of the function.
#'
#' @return
#' A new rap object, the result of applying the given rule.
#'
#' @seealso
#' `apply_rules` to apply more than one rule at the same time, `apply_rule` to apply one rule in a monoenvironmental P system.
#'
#' @section TODO:
#' * Print the trace of the execution, perhaps with the `RAPS::show_rap()` function.
#' * Include examples
#'
#' @export
apply_rule_menv = function(rap, rule_id, comp_id, verbose = FALSE, debug = FALSE) {
  # cat(crayon::bold("apply_rule_pdp() is under development"))
  cat("\nLaunching the rule with id", crayon::bold(rule_id), "in the compartment with id", crayon::bold(comp_id))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  #############################################
  # verbose = TRUE
  # debug = TRUE
  # comp_id = c_0
  # rule_id = i_0
  #############################################
  #############################################

  affected_rap = rap
  local_rule_id = rule_id
  affected_comps = affected_rap$Rules %>%
    dplyr::filter(rule_id == local_rule_id) %$%
    affected %>%
    magrittr::extract2(1)
  affected_rap$Configuration %<>% dplyr::filter(id %in% affected_comps)

  affected_rap %<>%
  # affected_rap %>% # Debugging
    RAPS::apply_rule(rule_id, debug)

  ## Debugging
  # RAPS::show_rap(rap, focus_on = list("MEM" = 2:3, "OBJ"))

  rap$Configuration %<>%
  # rap$Configuration %>% # Debugging
    dplyr::filter(!id %in% affected_comps) %>%
    dplyr::bind_rows(affected_rap$Configuration)

  ## Debugging
  # rap$Configuration %>% dplyr::filter(id %in% 2:3) %$% objects

  return(rap)
}
