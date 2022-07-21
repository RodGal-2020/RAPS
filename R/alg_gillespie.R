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
alg_gillespie = function(rap, max_T = 100, return_middle_states = TRUE) {
  cat(crayon::italic("\n\talg_gillepie"), "is under develpment, returning", crayon::italic("(j, tau) = (0,1)"), "by default")

  ### DELETE THIS DEMO
  cat("\nUsing the demo rap...")
  rap = RAPS::path2rap()
  ###

  ##########################
  ##### INICIALIZATION #####
  ##########################
  simulation_time = 0
  rules = rap$Rules
  if (return_middle_states) {
    raps = list(rap)
    new_index = 2
  }


  #####################
  ##### ITERATION #####
  #####################
  while (simulation_time < max_T) {
    ## Update propensities
    rules$propensity = rules$propensity # WOW

    ## Execute Gillespie algorithm with the new propensities
    exit_rule = RAPS::alg_gillespie_kernel(rules) # Written as an independent function for clearness

    ## Execute ONCE the given rule r_j_0
    rap %<>% # rap is modified
      RAPS::apply_rule(rule_id = exit_rule$j_c, membrane_id = NULL) # TODO: Check membrane_id

    ## Update simulation_time
    simulation_time %<>% sum(exit_rule$tau_c)

    ## Append new state
    if (return_middle_states) {
      raps %<>% append(rap)
      new_index %<>% sum(1)
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
