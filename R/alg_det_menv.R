#' Deterministic waiting time algorithm for multienvironmental systems
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Simulates the evolution of a P system given as a `rap` object using the deterministic waiting time algorithm.
#' @param rap A `rap` object, usually generated with `load_demo_dataset()` or `path2rap()`.
#' @param max_T Maximum simulation time.
#' @param verbose The verbosity, between 0 and 5.
#' @param debug Useful if you want to debug an execution of the function.
#' @param debug_trinity The same as `debug` but applied only to the debugging of the generation of the trinities.
#' @param random_trinity_selection If there are more than one possible trinity, do you want to select one randomly (TRUE) or do you want to select the first one according to `dplyr::arrange()` (FALSE)?
#' @param save_each A `function(rap)`. If not `NULL`, a list of `function(rap)` is returned.
#'
#' @return
#' A new `rap` object, the result of simulating the multienvironmental deterministic algorithm.
#'
#' @examples
#' fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua-5.0/RAPS/BIG/FAS.xml"
#' my_rap = path2rap(fas_path)
#' new_rap = alg_det_menv(my_rap, verbose = 4)
#' show_rap(new_rap)
#'
#' @section TODO:
#' - Add references.
#' - Improve efficiency saving the trinities.
#'
#' @export
alg_det_menv = function(rap, max_T = 1e-5, verbose = 2, debug = FALSE, debug_trinity = FALSE, random_trinity_selection = FALSE, save_each = NULL) {
  cat(crayon::bold("alg_det_menv() has not been validated yet\n"))

  cat(crayon::bold("WARNING: Using RAPS::get_propensities()\n"))
  rap$Rules = RAPS::get_propensities(rap, verbose = 0, debug = FALSE)

  if (debug) {
    cat("\n\tDebug: Remember that environmental movement rules are not supported... for now at least")
  }

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  #############################################
  # library(RAPS)
  # cat(crayon::bold("Using demo mode\n"))
  # verbose = TRUE
  # debug = TRUE
  # debug_trinity = FALSE # Too much
  # verbose = 5
  # max_T = 1e-5
  # random_trinity_selection = FALSE
  # save_each = NULL
  #############################################
  #############################################
  ###### FAS
  # cat(crayon::bold("Working with FAS in demo mode\n"))
  ## Using load_demo_dataset
  # rap = RAPS::load_demo_dataset("FAS")
  ## Using the PL5 XML
  # rap = RAPS::path2rap("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua-5.0/RAPS/BIG/FAS.xml")
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

  ##############################################################################
  drop_affected = function(rap) {
    rap$Rules %<>%
      dplyr::select(-affected)
    return(rap)
  }
  ##############################################################################

  ########################################
  get_trinities = function(rap, comps, debug = FALSE) {
    ## Debugging
    # comps = affected_comps
    # debug = TRUE
    if (debug) {
      init = Sys.time()
      cat("\n\tDebug: Computing trinities...")
      cat("\n", rep("=", 50), sep = "")
    }

    trinities = tibble::tibble(i = NULL,
                               tau_i = NULL,
                               c = NULL)

    for (comp in comps) {
      chosen_comp = rap$Configuration %>%
        dplyr::filter(id == comp)

      if (debug) {
        cat("\n", rep("-", 50), sep = "")
        cat("\n\tDebug: Focusing on comp", crayon::bold(comp), "\n")
        cat(rep("-", 50), sep = "")
        cat("\n\t\tDebug: Computing trinities for rules: ")
      }

      ## For each rule affecting chosen_comp
      # We define that a rule affects chosen_comp if it's its main membrane
      for (rule in 1:n_rules) {
        ## Debugging
        # rule = 1
        if (chosen_comp$id != rules$main_membrane_label[rule]) {
          # It doesn't affect, so we ignore it
          next
        }

        ## Debugging
        # rule = 1
        if (debug) {
          cat(rule, ", ", sep = "")
        }

        prod_concentration_of_reactives = 1

        ################################################
        ## TODO:
        # Use the RAPS::get_concentration_of() function.
        ################################################

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
            chosen_objects = rap$Configuration %>%
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
            chosen_objects = rap$Configuration %>%
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

        # TODO: Case EGFR*2

        # v_r = rap$Rules$propensity[rule] * prod_concentration_of_reactives # TODO: Check this
        v_r = rap$Rules$stochastic_constant[rule] * prod_concentration_of_reactives

        trinities %<>% dplyr::bind_rows(
          tibble::tibble(
            i = rules[rule, ]$rule_id,
            tau_i = ifelse(v_r != 0, 1 / v_r, Inf),
            c = comp
          )
        )
      }
    }

    if (debug) {
      elapsed_time = (Sys.time() - init) %>% round(2)
      cat("\n", rep("-", 50), sep = "")
      cat("\n\tDebug: Trinities computed after", crayon::bold(elapsed_time), "seconds" )
      cat("\n", rep("=", 50), sep = "")
    }
    return(trinities)
  }

  ########################################
  # Deterministic waiting time algorithm
  ########################################
  start_time = Sys.time()
  rules = rap$Rules
  n_rules = dim(rules)[1]
  comps = unique(rap$Configuration$id)

  simulation_time = 0
  verbose_print(cat(crayon::bold("\nsimulation_time"), simulation_time), 1)


  if (!is.null(save_each)) {
    saved = list(save_each(rap %>% drop_affected()))
  }

  trinities = get_trinities(rap, comps, debug_trinity)

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
    if (!random_trinity_selection) {
      chosen_trinity = trinities %>%
        dplyr::top_n(tau_i, n = -1) %>%
        magrittr::extract(1, )
    } else {
      chosen_trinity = trinities %>%
        dplyr::sample_n(1)
      magrittr::extract(1, )
    }

    i_0 = chosen_trinity[[1]]
    tau_i_0 = chosen_trinity[[2]]
    c_0 = chosen_trinity[[3]]

    verbose_print(cat("\n", rep("-", 50), sep = ""), 1)
    verbose_print(cat("\nWe have chosen the rule", crayon::bold(i_0), "with waiting time", tau_i_0, "to be executed in compartment", crayon::bold(c_0)), 1)

    verbose_print(RAPS::show_rule(dplyr::filter(rules, rule_id == i_0)), 3)

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
    rap %<>% RAPS::apply_rule_menv(rule_id = i_0,
                                   comp_id = c_0,
                                   debug)

    if (!is.null(save_each)) {
      saved %<>% append(list(save_each(rap %>% drop_affected())))
    }


    ## For each compartment affected by r_i_0
    affected_comps = rules %>%
      dplyr::filter(rule_id == i_0) %$%
      affected %>%
      magrittr::extract2(1)
    n_affected_comps = length(affected_comps)

    ## Delete trinities of the affected_comps
    trinities %<>%
      dplyr::filter(!(c %in% affected_comps))

    ## Update multiplicities of objects in c'
    # Already done in the "Apply rule" step

    ## Compute new waiting times for affected_comps
    # Made inside the following get_trinities() function

    ## Add new trinities for affected_comps
    # rap$Rules = RAPS::get_propensities(rap, verbose = 0, debug = FALSE) # Not necessary, as we work only with the sc
    new_trinities = get_trinities(rap, affected_comps, debug_trinity)
    trinities %<>%
      dplyr::bind_rows(new_trinities)

    ## Order by increasing tau_i
    trinities %<>%
      dplyr::arrange(tau_i) # Increasing order

    ## End for c compartment

    time_now = Sys.time()
    elapsed_time = as.numeric(time_now - start_time)

    verbose_print(cat("\nElapsed time:", elapsed_time, "seconds."), 1)

    cat("\n")

  } # End of main iteration

  ########################
  ##### END FUNCTION #####
  ########################

  end_time = Sys.time()
  total_elapsed_time = as.numeric(end_time - start_time)
  verbose_print(cat("\n", rep("=", 50), sep = ""), 1)
  verbose_print(cat("\nTotal elapsed time:", total_elapsed_time), 1)
  verbose_print(cat("\n\n", crayon::bold("Reached end of simulation\n"), rep("=", 50), sep = ""), 1)


  if (is.null(save_each)) {
    return(rap)
  } else {
    return(list(
      final_rap = rap,
      selected_data = saved
    ))
  }

}

