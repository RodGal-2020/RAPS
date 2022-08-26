#' Gillespie's algorithm for monoenvironmental systems
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Simulates the evolution of a P system given as a `rap` object using the Gillespie's algorithm for monoenvironmental systems
#' @param rap A `rap` object, usually generated with `load_demo_dataset()` or `path2rap()`.
#' @param max_T Maximum simulation time.
#' @param return_middle_states Useful if you want to debug an execution of the function.
#' @param verbose The verbosity, between 0 and 5.
#' @param propensity_function A function to update propensities during each step
#'
#' @return A new `rap` object, the result of simulating the Gillespie's algorithm for monoenvironmental systems.
#'
#' @examples
#' fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml"
#' my_rap = path2rap(fas_path)
#' new_rap = alg_gillespie (my_rap, verbose = 4)
#' show_rap(new_rap)
#'
#' @section TODO:
#' Add references.
#'
#' @export
alg_gillespie = function(rap, max_T = 10, propensity_function = NULL, return_middle_states = TRUE, verbose = FALSE) {
  ### DELETE THIS DEMO
  # cat("\nUsing the demo rap...")
  # rap = RAPS::path2rap(demo = 2)
  # max_T = 10
  # propensity_function = NULL
  # return_middle_states = TRUE
  # verbose = TRUE
  ###

  if (is.null(propensity_function) & verbose){
    cat("\nUsing constant propensities")
  }

  ##########################
  ##### INICIALIZATION #####
  ##########################
  simulation_time = 0
  rules = rap$Rules
  if (return_middle_states) {
    raps = list(rap$Configuration)
  }


  #####################
  ##### ITERATION #####
  #####################
  while (simulation_time < max_T) {
    ## Update propensities
    if (is.null(propensity_function)) {
      rules$propensity = rules$propensity # WOW
    } else {
      rules$propensity = propensity_function(rap)
    }

    ## Execute Gillespie algorithm with the new propensities
    exit_rule = RAPS::alg_gillespie_kernel(rules) # Written as an independent function for clearness
    if (verbose) {
      cat("\tThe chosen rule is the one with id =", exit_rule$j_c, "and execution time tau =", exit_rule$tau_c)
    }

    ## Execute ONCE the given rule r_j_0
    rap %<>% # rap is modified
      RAPS::apply_rule(rule_id = exit_rule$j_c) # TODO: Check membrane_id

    ## Update simulation_time
    simulation_time %<>% sum(exit_rule$tau_c)
    if (verbose) {
      cat("Execution time ", simulation_time, "out of", max_T)
    }

    ## Append new state
    if (return_middle_states) {
      raps %<>% append(rap)
    }
  }


  ######################
  ######## END #########
  ######################
  if (return_middle_states) {
    return(raps)
  } else {
    return(rap)
  }
}
