#' Deterministic waiting time algorithm for multienvironmental systems
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Simulates the evolution of a P system given as a `rap` object using the deterministic waiting time algorithm.
#' @param rap A `rap` object, usually generated with `load_demo_dataset()` or `path2rap()`.
#' @param max_T Maximum simulation time.
#' @param verbose The verbosity, between 0 and 5.
#' @param debug Useful if you want to debug an execution of the function.
#' @param debug_trinity The same as `debug` but applied only to the debugging of the generation of the trinities.
#' @param random_trinity_selection If there are more than one possible trinity, do you want to select one randomly (TRUE) or do you want to select the first one according to `dplyr::arrange()` (FALSE)?
#'
#' @return
#' A new `rap` object, the result of simulating the multienvironmental deterministic algorithm.
#'
#' @examples
#' fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml"
#' my_rap = path2rap(fas_path)
#' new_rap = alg_det_menv(my_rap, verbose = 4)
#' show_rap(new_rap)
#'
#' @section TODO:
#' Add references.
#'
#' @export
alg_det_menv = function(rap, max_T = 1e-5, verbose = 2, debug = FALSE, debug_trinity = FALSE, random_trinity_selection = FALSE) {
  cat(crayon::bold("alg_det_menv() is under development\n"))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  #############################################
  # library(RAPS)
  # cat(crayon::bold("Using demo mode\n"))
  # verbose = TRUE
  # debug = TRUE
  # debug_trinity = FALSE
  # verbose = 5
  # max_T = 1e-5
  # random_trinity_selection = FALSE
  #############################################
  #############################################
  ###### FAS
  # cat(crayon::bold("Working with FAS in demo mode\n"))
  ## Using load_demo_dataset
  # rap = RAPS::load_demo_dataset("FAS")
  # # Using the PL5 XML
  # rap = RAPS::path2rap("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml")
  ###### multi_communication from path2rap
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/2%20-%20multi_inside_to_multi_outside_r.xml"
  # rap = path2rap(path)
  #############################################


  ##############################################################################
  verbose_print = function(action, minimum_verbose_to_print = 1) {
    if (verbose >= minimum_verbose_to_print) {
      action
    }
  }
  ##############################################################################

  ########################################
  get_trinities = function(envs, debug = FALSE) {
    ## Debugging
    # envs = unique(rap$Configuration$environment)
    # debug = TRUE

    verbose_print(cat("\nComputing trinities"), 2)

    trinities = tibble::tibble(i = NULL,
                               tau_i = NULL,
                               c = NULL)
    for (env in envs) {
      # # Debugging
      # env = envs[1]
      chosen_env = rap$Configuration %>%
        dplyr::filter(environment == env)

      if (debug) {
        cat("\n\n", rep("-", 50), sep = "")
        cat("\n\tDebug: Focusing on env", crayon::bold(env), "\n")
        cat(rep("-", 50), "\n", sep = "")
      }

      for (rule in 1:n_rules) {
        # # Debugging
        # rule = 1
        if (debug) {
          cat("\n\tDebug: Computing trinity for rule", rule, "with id =", rules[rule, ]$rule_id)
          # RAPS::show_rule(rules[rule, ])
        }
        prod_concentration_of_reactives = 1
        main_membrane_label = rules[rule, ]$main_membrane_label
        lhs = rules[rule, ]$lhs[[1]] %>%
          dplyr::filter(where != "@exists")
        # Membranes' concentration is not considered
        n_reactives = dim(lhs)[1]
        for (reactive in 1:n_reactives) {
          ## Debugging
          # reactive = 1
          where = lhs$where[reactive]
          if (where == "@here") {
            chosen_objects = chosen_env %>%
              dplyr::filter(id == main_membrane_label) %$%
              objects

            if (length(chosen_objects) != 0) {
              old_concentration = chosen_objects %>%
                magrittr::extract2(1) %>%
                dplyr::filter(object == lhs$object[reactive]) %$%
                multiplicity %>%
                sum(0) # If is not found it becomes "numeric(0)", and with this a real 0
            } else {
              old_concentration = 0
            }

          } else {
            # It is in some other membrane "h"
            chosen_objects = chosen_env %>%
              dplyr::filter(id == where) %$%
              objects

            if (length(chosen_objects) != 0) {
              old_concentration = chosen_objects %>%
                magrittr::extract2(1) %>%
                dplyr::filter(object == lhs$object[reactive]) %$%
                multiplicity %>%
                sum(0) # If is not found it becomes "numeric(0)", and with this a real 0
            } else {
              old_concentration = 0
            }
          }
          prod_concentration_of_reactives %<>%
            prod(old_concentration)
        }
        v_r = propensities[rule] * prod_concentration_of_reactives

        trinities %<>% dplyr::bind_rows(
          tibble::tibble(
            i = rules[rule, ]$rule_id,
            tau_i = ifelse(v_r != 0, 1 / v_r, Inf),
            c = env
          )
        )
      }
    }

    return(trinities)
  }

  ########################################
  # Deterministic waiting time algorithm
  ########################################
  simulation_time = 0
  verbose_print(cat(crayon::bold("\nsimulation_time"), simulation_time), 1)
  envs = unique(rap$Configuration$environment) # Instead of max in order to generalize
  rules = rap$Rules
  n_rules = dim(rules)[1]
  propensities = rules$propensity


  trinities = get_trinities(envs, debug_trinity)

  ## Order by increasing tau_i
  trinities %<>%
    dplyr::arrange(tau_i) # Increasing order

  if (debug) {
    cat("\n\tDebug: Generated trinities:\n")
    print(trinities)
  }


  #########################
  ####### ITERATION #######
  #########################
  while (simulation_time < max_T) {

    ## Get the first trinity
    if (random_trinity_selection) {
      chosen_trinity = trinities %>%
        dplyr::sample_n(1)
        magrittr::extract(1, )
    } else {
      chosen_trinity = trinities %>%
        dplyr::top_n(tau_i, n = -1) %>%
        magrittr::extract(1, )
    }

    i_0 = chosen_trinity[[1]]
    tau_i_0 = chosen_trinity[[2]]
    c_0 = chosen_trinity[[3]]

    verbose_print(cat("\nWe have chosen the rule", crayon::bold(i_0), "with waiting time", tau_i_0, "to be executed in environment", crayon::bold(c_0)), 1)

    verbose_print(RAPS::show_rule(dplyr::filter(rap$Rules, rule_id == i_0)), 3)

    ## Delete the chosen trinity from the trinities' list
    trinities %<>%
      dplyr::filter(i != i_0 | c != c_0)

    ## Update simulation time
    simulation_time %<>% sum(tau_i_0)
    verbose_print(cat(crayon::bold("\nsimulation_time"), simulation_time), 1)

    ## Update waiting time in other trinities
    trinities %<>%
      dplyr::mutate(tau_i = tau_i - tau_i_0)

    ## Apply rule r_i_0 ONCE

    ## Debugging
    # rap %>% RAPS::apply_rule_menv(rule_id = i_0,
    rap %<>% RAPS::apply_rule_menv(rule_id = i_0,
                                   environment_id = c_0,
                                   debug)

    ## For each environment affected by r_i_0
    affected_environments = c(c_0)
    n_affected_environments = length(affected_environments)
    if (debug) {
      cat("\n\tDebug: Remember that environmental movement rules are not supported... for now 😈")
    }

    ## Delete trinities of the affected_environment
    trinities %<>%
      dplyr::filter(!(c %in% affected_environments))

    ## Update multiplicities of objects in c'
    # Already done in the "Apply rule" step

    ## Compute new waiting times for affected_environment
    # Made inside the get_trinities() function

    ## Add new trinities for affected_environment
    new_trinities = get_trinities(affected_environments, debug_trinity)
    trinities %<>%
      dplyr::bind_rows(new_trinities)

    ## Order by increasing tau_i
    trinities %<>%
      dplyr::arrange(tau_i) # Increasing order

  } # End of main iteration

  ########################
  ##### END FUNCTION #####
  ########################
  return(rap)
}

