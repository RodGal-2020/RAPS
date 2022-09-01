#' Apply a rule to a given multienvironmental P system
#'
#' `r lifecycle::badge("experimental")`
#' Apply a rule to a given multienvironmental P system given as a `rap` object.
#'
#' @param rap A rap object.
#' @param rule_id The id of the rule to be applied.
#' @param environment_id The id of the affected environment.
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
apply_rule_menv = function(rap, rule_id, environment_id = 0, verbose = FALSE, debug = FALSE) {
  # cat(crayon::bold("apply_rule_pdp() is under development"))
  cat("\nLaunching the rule with id", crayon::bold(rule_id), "in the environment with id", crayon::bold(environment_id))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  #############################################
  # cat(crayon::bold("Using demo mode\n"))
  # verbose = TRUE
  # debug = TRUE
  #############################################
  #############################################
  ###### FAS
  # rap = RAPS::load_demo_dataset("FAS")
  # environment_id = "e"
  # rule_id = 96
  # cat(crayon::bold("Working with FAS in demo mode\n"))
  ###### demo 1 from path2rap
  # rap = RAPS::path2rap(demo = 1)
  # environment_id = "0"
  # rule_id = "In-out"
  #############################################
  #############################################
  ###### alg_det_menv
  # rule_id = i_0
  # environment_id = c_0


  affected_rap = rap
  affected_rap$Configuration %<>% dplyr::filter(environment == environment_id)

  ## Debugging
  # affected_rap %>%
  #   RAPS::apply_rule(rule_id, debug)

  affected_rap %<>%
    RAPS::apply_rule(rule_id, debug)

  ## Debugging
  # RAPS::show_rap(rap, focus_on = list("MEM" = 2:3, "OBJ"))

  rap$Configuration %<>%
    dplyr::filter(environment != environment_id) %>%
    dplyr::bind_rows(affected_rap$Configuration)

  ## Debugging
  # rap$Configuration %>% dplyr::filter(id %in% 2:3) %$% objects

  return(rap)
}
