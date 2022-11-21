#' Gillespie's algorithm for monoenvironmental systems
#'
#' @description
#' `r lifecycle::badge("stable")`
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
#' fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua-5.0/RAPS/BIG/FAS.xml"
#' my_rap = path2rap(fas_path)
#' new_rap = alg_gillespie (my_rap, verbose = 4)
#' show_rap(new_rap)
#'
#' @section TODO:
#' Add references.
#'
#' @export
alg_gillespie = function(rap, max_T = 10, propensity_function = RAPS::get_propensities, verbose = 2, debug = FALSE, debug_pair = FALSE, random_pair_selection = FALSE, save_each = NULL) {
  #################################################
  ## Debugging
  # library(RAPS)
  # fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua-5.0/RAPS/BIG/FAS.xml"
  # rap = path2rap(fas_path)
  # max_T = 1e-5
  # propensity_function = RAPS::get_propensities
  # verbose = 5
  # debug = FALSE
  # debug_pair = FALSE
  # random_pair_selection = FALSE
  # save_each = NULL
  #################################################

  ##############################################################################
  verbose_print = function(action, minimum_verbose_to_print = 1) {
    if (verbose >= minimum_verbose_to_print) {
      action
    }
  }
  ##############################################################################

  if (is.null(propensity_function) & verbose){
    cat("\nUsing constant propensities")
  } else {
    cat("\nUpdating propensities each step")
  }

  ##########################
  ##### INICIALIZATION #####
  ##########################
  simulation_time = 0
  if (!is.null(save_each)) {
    saved = list(save_each(rap))
  }


  #####################
  ##### ITERATION #####
  #####################
  while (simulation_time < max_T) {
    ## Update propensities
    if (!is.null(propensity_function)) {
      rap$Rules = propensity_function(rap)
    } # Else we keep them constant

    ## Execute Gillespie algorithm with the new propensities
    exit_rule = RAPS::alg_gillespie_kernel(rap) # Written as an independent function for clearness
    if (verbose) {
      cat("\tThe chosen rule is the one with id =", exit_rule$i, "and execution time tau =", exit_rule$tau)
    }

    ## Execute ONCE the given rule r_j_0
    rap %<>% RAPS::apply_rule(rule_id = exit_rule$i)

    ## Update simulation_time
    simulation_time %<>% sum(exit_rule$tau)
    verbose_print(cat("\nExecution time ", simulation_time, "out of", max_T), 1)
    verbose_print(cat(rep("-", 50), sep = ""), 1)

    ## Append new state
    if (!is.null(save_each)) {
      saved %<>% append(list(rap))
    }
  }


  ######################
  ######## END #########
  ######################
  if (is.null(save_each)) {
    return(rap)
  } else {
    return(list(
      final_rap = rap,
      selected_data = saved
    ))
  }
}
