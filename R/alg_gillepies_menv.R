#' Gillespie's algorithm for multienvironmental systems
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' **Under development**.
#'
#' Simulates the evolution of a P system given as a `rap` object using the Gillespie's algorithm for multienvironmental systems
#' @param rap A `rap` object, usually generated with `load_demo_dataset()` or `path2rap()`.
#' @param max_T Maximum simulation time.
#' @param verbose The verbosity, between 0 and 5.
#' @param debug Useful if you want to debug an execution of the function.
#'
#' @return
#' A new `rap` object, the result of simulating the Gillespie's algorithm for multienvironmental systems.
#'
#' @examples
#' fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml"
#' my_rap = path2rap(fas_path)
#' # new_rap = alg_gillespie_menv(my_rap, verbose = 4)
#' # show_rap(new_rap)
#'
#' @section CAUTION:
#' For now affected compartment is equal to every compartment, like in the deterministic waiting time algorithm.
#'
#' @export
alg_gillespie_menv = function(rap, max_T = 1e-5, verbose = 2, debug = FALSE) {
  cat(crayon::bold("alg_gillespie_menv() is under development"))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  # library(RAPS)
  # debug = TRUE
  # verbose = 3
  # max_T = 1e-1
  # # RAP object
  # rap = RAPS::load_demo_dataset("FAS")
  # new_env = rap$Configuration %>%
  #   dplyr::mutate(environment = "second_environment")
  # rap$Configuration %<>%
  #   dplyr::bind_rows(new_env)
  ###

  ##############################################################################
  verbose_print = function(action, minimum_verbose_to_print = 1) {
    if (verbose >= minimum_verbose_to_print) {
      action
    }
  }
  ##############################################################################

  ##############################################################################
  get_ac = function(rap, env, mem, rule) {
    # rule = chosen_rule
    # compartment = c_0

    verbose_print(cat("\nGetting affected compartments by the application of the rule with id ", crayon::bold(rule$rule_id), " in compartment ", crayon::bold(paste0(env, "-", mem)),".", sep = ""), 3)

    # ac = rule$main_membrane_label
    #
    # aux = rule$rhs %>%
    #   dplyr::bind_rows(rule$lhs) %>%
    #   dplyr::filter(where != "@here") %$%
    #   where
    #
    # ac %<>% c(aux)
    #
    # n_ac = length(ac)
    #
    # for (i in 1:n_ac) {
    #   ac[i] %<>% paste0(env)
    # }

    case = "ALL"

    if (case == "RAND") {
      cat("\nReturning a", crayon::bold("random"), "selection of compartments of length 2.")
      ac = sample(rap$Configuration$compartment, size = 2, replace = FALSE)
    } else if (case == "ALL") {
      cat("\nReturning", crayon::bold("all"), "compartments as list.")
      ac = list(
        rap$Configuration$environment,
        rap$Configuration$id
      )
    } else {
      cat("\nReturning", crayon::bold("NULL"))
      return(NULL)
    }

    return(ac)
  }
  ##############################################################################

  simulation_time = 0
  start_time = Sys.time()
  verbose_print(cat(crayon::bold("\nsimulation_time"), simulation_time), 1)

  envs = rap$Configuration$environment %>% unique
  mems = rap$Configuration$id %>% unique
  # Add compartment id
  rap$Configuration %<>%
    dplyr::mutate(compartment = paste0(environment, "-", id))


  #####################
  ##### TRINITIES #####
  #####################
  # Actually, they are quintets
  verbose_print(cat("\nComputing trinities"), 2)

  trinities = tibble::tibble(i = NULL,
                             tau_i = NULL,
                             c = NULL,
                             env = NULL,
                             membrane = NULL)

  for (env in envs) {
    for (mem in mems) {
      # env = envs[1]
      # mem = mems[1]
      c = paste0(env, "-", mem)

      local_rap = rap
      local_rap$Configuration %<>%
        dplyr::filter(compartment == c)

      new_trinity = local_rap %>%
        RAPS::alg_gillespie_kernel() %>%
        dplyr::rename(tau_c = tau, i_c = i) %>%
        dplyr::bind_cols(c = c, env = env, mem = mem)
      trinities %<>%
        dplyr::bind_rows(new_trinity) # (c, i_c, tau_c)
    }
  }

  ## We don't need to arrange this, as we have the dplyr::top_n() function
  trinities %<>%
    dplyr::arrange(tau_c)

  if (debug) {
    print(trinities)
  }

  #####################
  ##### ITERATION #####
  #####################
  while (simulation_time < max_T) {

    ## Choose the trinity
    chosen_trinity = trinities[1, ]
    tau_c_0 = chosen_trinity$tau_c
    i_c_0 = chosen_trinity$i_c
    c_0 = chosen_trinity$c
    # The following are included for the sake of simplicity
    e_0 = chosen_trinity$env
    m_0 = chosen_trinity$mem
    chosen_rule = rap$Rules %>%
      dplyr::filter(rule_id == i_c_0)

    ## Delete the trinity
    trinities %<>%
      magrittr::extract(-1, )

    ## Update simulation time
    simulation_time %<>%
      sum(tau_c_0)
    verbose_print(cat(crayon::bold("\nsimulation_time"), simulation_time), 1)

    ## Update waiting time of other trinities
    trinities %<>%
      dplyr::mutate(tau_c = tau_c - tau_c_0)

    ## Apply rule r_i_c_0 ONCE actualizing the affected compartments
    verbose_print(cat("\nWe have chosen the rule", crayon::bold(i_c_0), "with waiting time", tau_c_0, "to be executed in environment", crayon::bold(c_0)), 1)
    rap %<>%
      RAPS::apply_rule_menv(rule_id = i_c_0, environment_id = e_0)

    ## For each affected compartment ac:
    acs_list = get_ac(rap, env = e_0, mem = m_0, rule = chosen_rule)

    cat("\nPropensities are not updated in this version. Take a look at", crayon::italic("alg_det_menv.R"), "for inspiration.")
    verbose_print(cat("\nComputing trinities for affected compartments"), 2)

    for (env in acs_list[[1]]) {
      for (mem in acs_list[[2]]) {
        ac = paste0(env, "-", mem)

        ### Delete trinity (j_ac, tau_ac, ac) from the list
        trinities %<>%
          dplyr::filter(c != ac)

        ### Update propensities

        ### Execute the kernel of the Gillespie algorithm in ac, obtaining (js_ac, tau_ac, ac) # s for Star
        local_rap = rap
        local_rap$Configuration %<>%
          dplyr::filter(compartment == ac)

        new_trinity = local_rap %>%
          RAPS::alg_gillespie_kernel() %>%
          dplyr::rename(tau_c = tau, i_c = i) %>%
          dplyr::bind_cols(c = c, env = env, mem = mem)

        ### Add the (js_ac, tau_ac, ac) trinity to the list
        trinities %<>%
          dplyr::bind_rows(new_trinity) # (c, i_c, tau_c)

        ### Order the new list by decreasing tau_c
        # We don't need to arrange this, as we have the dplyr::top_n() function
        trinities %<>%
          dplyr::arrange(tau_c)
      }
    }

    time_now = Sys.time()
    elapsed_time = as.numeric(time_now - start_time)
    verbose_print(cat("\nelapsed time:", elapsed_time), 1)

    cat("\n")
  }


  ########################
  ##### END FUNCTION #####
  ########################
  return(rap)
}
