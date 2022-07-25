#' Template for new functions
#'
#' `r lifecycle::badge('experimental')`
#' @param my_param Yep, it's a parameter.
#' @return It returns...
#' @examples
#' Some examples
#' @section Warning:
#' This is a warning
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
    raps = list(rap$RAP)
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
