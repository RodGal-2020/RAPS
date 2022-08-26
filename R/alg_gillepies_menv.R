#' Gillespie's algorithm for multienvironmental systems
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Simulates the evolution of a P system given as a `rap` object using the Gillespie's algorithm for multienvironmental systems
#' @param rap A `rap` object, usually generated with `load_demo_dataset()` or `path2rap()`.
#' @param max_T Maximum simulation time.
#'
#' @return
#' A new `rap` object, the result of simulating the Gillespie's algorithm for multienvironmental systems.
#'
#' @examples
#' fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml"
#' my_rap = path2rap(fas_path)
#' new_rap = alg_gillespie_menv(my_rap, verbose = 4)
#' show_rap(new_rap)
#'
#' @section TODO:
#' Add references.
#'
#' @export
alg_gillespie_menv = function(rap_environment, max_T = 10) {
  cat(crayon::bold("alg_gillespie_pdp() is under development"))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  # cat("\nUsing the demo rap...")
  # rap = RAPS::path2rap(demo = 2)
  # verbose = 1
  # debug = TRUE
  # new_environment = rap$RAP %>%
  #   dplyr::filter(label == 1) %>%
  #   dplyr::mutate(environment = 1)
  # rap$RAP %<>%
  #   dplyr::bind_rows(new_environment)
  # rule_id = 1 # To track errors
  ###


  ########################################
  # Gillespie algorithm
  ########################################
  simulation_time = 0
  prov_RAP = rap$RAP

  n_envs = prov_RAP %$%
    environment %>%
    max()

  # grouped_RAP = prov_RAP %>%
  #   dplyr::group_by(environment)

  gil_exit = tibble::tibble()

  for (i in 0:n_envs) {
    new_gil_exit = prov_RAP %>%
      dplyr::filter(environment == i) %>%
      RAPS::alg_gillespie() %>%
      dplyr::bind_cols(c = i)
    gil_exit %<>%
      dplyr::bind_rows(new_gil_exit) # (j_c, tau_c)
  }

  ### DELETE THIS DEMO
  gil_exit[2,] = list(1, 2, 3)
  ###

  #####################
  ##### ITERATION #####
  #####################
  while (simulation_time < max_T) {

    ## We don't need to arrange this, as we have the dplyr::top_n() function
    gil_exit %<>%
      dplyr::arrange(tau_c)

    ## Choose the trinity
    chosen_trinity = gil_exit[1, ]
    tau_c_0 = chosen_trinity$tau_c
    j_c_0 = chosen_trinity$j_c
    c_0 = chosen_trinity$c

    ## Delete the trinity
    gil_exit %<>%
      magrittr::extract(-1, )

    # chosen_trinity = gil_exit %>%
    #   dplyr::top_n(1, dplyr::desc(tau_c))

    ## Update simulation time
    simulation_time %<>%
      sum(tau_c_0)

    ## Update waiting time of other trinities
    gil_exit %<>%
      dplyr::mutate(tau_c = tau_c - tau_c_0)

    ## TODO: Apply rule r_j_c_0 ONCE actualizing the affected environments
    # prov_RAP %<>%
    #   RAPS::apply_rule()

    ## For each cp affected environment:

    ### Delete trinity (j_cp, tau_cp, cp) from the list

    ### Update propensities

    ### Execute alg_gillespie in cp, obtaining (js_cp, taus_cp, cp) # s for Star

    ### Add the (js_cp, taus_cp, cp) trinity to the list
    # gil_exit

    ### Order the new list by decreasing tau_c
    ## We don't need to arrange this, as we have the dplyr::top_n() function

    ########################
    #####  UPDATE RAP  #####
    ########################
    rap$RAP = prov_RAP
  }


  ########################
  ##### END FUNCTION #####
  ########################
  return(rap)
}
