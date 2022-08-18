#' Apply a rule to a given rap object
#'
#' `r lifecycle::badge("experimental")`
#' @param rap A rap object.
#' @param rule_id The id of the rule to be applied.
#' @param ... Other parameters such as `debug` or `verbose`.
#' @return A new rap object, the result of applying the given rule.
#' @examples
#' TODO
#' @seealso
#' `apply_rules` to apply more than one rule at the same time.
#' #' @section TODO:
#' * Print the trace of the execution, perhaps with the `RAPS::show_rap()` function.
#' @export
apply_rule_menv = function(rap, rule_id, environment_id = 0, verbose = FALSE, debug = FALSE) {
  # cat(crayon::bold("apply_rule_pdp() is under development"))
  cat("\nLaunching the rule with id", crayon::bold(rule_id), "in the environment with id", crayon::bold(environment_id), "\n")

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  #############################################
  cat(crayon::bold("Using demo mode\n"))
  verbose = TRUE
  debug = TRUE
  #############################################
  #############################################
  ###### FAS
  # rap = RAPS::load_demo_dataset("FAS")
  # environment_id = "e"
  # rule_id = 96
  # cat(crayon::bold("Working with FAS in demo mode\n"))
  ###### demo 1 from path2rap
  rap = RAPS::path2rap(demo = 1)
  environment_id = "0"
  rule_id = "In-out"
  #############################################

  affected_rap = rap
  affected_rap$Configuration %<>% dplyr::filter(environment == environment_id)

  affected_rap %>% # Debugging
    RAPS::apply_rule(rule_id, debug)

  affected_rap %<>%
    RAPS::apply_rule(rule_id, debug)

  rap$Configuration %>%
    dplyr::filter(environment != environment_id) %>%
    dplyr::bind_rows(affected_rap$Configuration)

  return(rap)
}
