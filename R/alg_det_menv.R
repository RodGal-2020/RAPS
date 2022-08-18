#' Template for new functions
#'
#' This is a template.
#' @param my_param Yep, it's a parameter.
#' @return It returns...
#' @examples
#' Some examples
#' @section Warning:
#' This is a warning
#' @export
alg_det_menv = function(rap, max_T = 10, verbose = TRUE, debug = FALSE) {
  cat(crayon::bold("alg_det_menv() is under development\n"))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  #############################################
  cat(crayon::bold("Using demo mode\n"))
  verbose = TRUE
  debug = TRUE
  max_T = 1
  #############################################
  #############################################
  ###### FAS
  # rap = RAPS::load_demo_dataset("FAS")
  # cat(crayon::bold("Working with FAS in demo mode\n"))
  ###### demo 1 from path2rap
  # rap = RAPS::path2rap(demo = 1)
  ###### demo "small" from path2rap
  rap = RAPS::path2rap(demo = "small")
  #############################################


  ########################################
  # Deterministic waiting time algorithm
  ########################################
  simulation_time = 0
  cat(crayon::bold("\nsimulation_time"), simulation_time)
  envs = unique(rap$Configuration$environment) # Instead of max in order to generalize
  rules = rap$Rules
  n_rules = dim(rules)[1]
  propensities = rules$propensity

  cat("\nComputing trinities")
  trinities = tibble::tibble(i = NULL,
                             tau_i = NULL,
                             c = NULL)
  for (env in envs) {
    for (rule in 1:n_rules) {
      if (debug) {
        cat("\n\tDebug: Computing trinity for rule", rule, "\n")
        # RAPS::show_rule(rules[rule, ])
      }
      prod_concentration_of_reactives = 1
      main_membrane_label = rules[rule, ]$main_membrane_label
      lhs = rules[rule, ]$lhs[[1]] %>%
        dplyr::filter(where != "@exists")
      n_reactives = dim(lhs)[1]
      for (reactive in 1:n_reactives) {
        where = lhs$where[reactive]
        if (where == "@here") {
          old_concentration = rap$Configuration %>%
            dplyr::filter(id == main_membrane_label) %$%
            objects %>%
            magrittr::extract2(1) %>%
            dplyr::filter(object == lhs$object[reactive]) %$%
            multiplicity %>%
            sum(0) # If is not found it becomes "numeric(0)", and with this a real 0
        } else {
          # It is in some other membrane "h"
          old_concentration = rap$Configuration %>%
            dplyr::filter(id == where) %$%
            objects %>%
            magrittr::extract2(1) %>%
            dplyr::filter(object == lhs$object[reactive]) %$%
            multiplicity %>%
            sum(0)
        }
        prod_concentration_of_reactives %<>%
          prod(old_concentration)
      }
      v_r = propensities[rule] * prod_concentration_of_reactives

      trinities %<>% dplyr::bind_rows(
        tibble::tibble(
          i = rule,
          tau_i = ifelse(v_r != 0, 1 / v_r, 1e-6),
          c = env
        )
      )
    }
  }

  if (debug) {
    cat("\n\tDebug: Generated trinities:\n")
    print(trinities)
  }

  ## Order by increasing tau_i
  # trinities %<>%
  #   dplyr::arrange(tau_i) # Increasing order



  #########################
  ####### ITERATION #######
  #########################
  while (simulation_time < max_T) {

    ## Get the first trinity
    chosen_trinity = trinities %>%
      dplyr::top_n(tau_i, n = -1) %>%
      # dplyr::sample_n(1) # Random alternative
      magrittr::extract(1, )

    i_0 = chosen_trinity[[1]]
    tau_i_0 = chosen_trinity[[2]]
    c_0 = chosen_trinity[[3]]
    if (verbose) {
      cat("\nWe have chosen the rule", crayon::bold(i_0), "with waiting time", tau_i_0, "to be executed in environment", crayon::bold(c_0))
      # rap$Rules %>%
      #   dplyr::filter(rule_id == i_0) %>%
      #   RAPS::show_rule()
    }

    ## Delete the chosen trinity from the trinities' list
    trinities %<>%
      dplyr::filter(i != i_0 | c != c_0)

    ## Update simulation time
    simulation_time %<>% sum(tau_i_0)
    cat(crayon::bold("\nsimulation_time"), simulation_time)

    ## Update waiting time in other trinities
    trinities %<>%
      dplyr::mutate(tau_i = tau_i - tau_i_0)

    ## Apply rule r_i_0 ONCE
    rap %<>% RAPS::apply_rule_menv(rule_id = i_0,
                                   environment_id = c_0,
                                   debug)

    ## For each environment affected by r_i_0
    affected_environments = c(c_0)
    n_affected_environments = length(affected_environments)
    if (debug) {
      cat("\n\tDebug: Remember that environmental movement rules are not supported... for now 😈")
    }

    for (affected_environment in affected_environments) {
      ## Delete trinities of the affected_environment
      trinities %<>%
        dplyr::filter(c != affected_environment)

      ## Update multiplicities of objects in c'
      # Already done in the "Apply rule" step

      ## Compute new waiting times for affected_environment
      # cat("\nUsing concentration_of_reactives = 1:n_rules")
      # concentration_of_reactives = 1:n_rules # It depends on the environment (!)

      ## Add new trinities for affected_environment
      for (rule in 1:n_rules) {
        if (debug) {
          cat("\n\tDebug: Re-computing trinity for rule", rule, "for affected_environment", affected_environment, "\n")
          # RAPS::show_rule(rules[rule, ])
        }
        prod_concentration_of_reactives = 1
        main_membrane_label = rules[rule, ]$main_membrane_label
        lhs = rules[rule, ]$lhs[[1]] %>%
          dplyr::filter(where != "@exists")
        n_reactives = dim(lhs)[1]
        for (reactive in 1:n_reactives) {
          where = lhs$where[reactive]
          if (where == "@here") {
            new_concentration = rap$Configuration %>%
              dplyr::filter(id == main_membrane_label) %$%
              objects %>%
              magrittr::extract2(1) %>%
              dplyr::filter(object == lhs$object[reactive]) %$%
              multiplicity %>%
              sum(0) # If is not found it becomes "numeric(0)", and with this a real 0
          } else {
            # It is in some other membrane "h"
            new_concentration = rap$Configuration %>%
              dplyr::filter(id == where) %$%
              objects %>%
              magrittr::extract2(1) %>%
              dplyr::filter(object == lhs$object[reactive]) %$%
              multiplicity %>%
              sum(0)
          }
          prod_concentration_of_reactives %<>%
            prod(new_concentration)
        }
        v_r = propensities[rule] * prod_concentration_of_reactives

        trinities %<>% dplyr::bind_rows(
          tibble::tibble(
            i = rule,
            tau_i = ifelse(v_r != 0, 1 / v_r, 1e6),
            c = affected_environment
          )
        )
      }

      ## Order by increasing tau_i
      # trinities %<>%
      #   dplyr::arrange(tau_i) # Increasing order
    }
  } # End of iteration

  ########################
  ##### END FUNCTION #####
  ########################
  return(rap)
}

