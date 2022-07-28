#' Choose a rule to be applied in a rap object
#'
#' Note that the rule is not applied.
#' @param rap A rap object.
#' @param semantics The path to a semantics object, or the id of a given algorithm from the following list: GIL (for Gillespie), DET (for Deterministic Waiting Times Algorithm).
#' @return The id of a rule which is to be applied and the id of the chosen membrane.
#' @examples
#' TODO.
#' @export
choose_rule = function(rap, semantics = "GIL") {
  cat(crayon::italic("\n\tchoose_rule"), "is under develpment, returning", crayon::italic("rule_id = 1, membrane_id = 1"), "by default")

  ### DELETE THIS DEMO
  cat("\nUsing the demo rap...")
  rap = RAPS::path2rap()
  new_environment = rap$Configuration %>%
    magrittr::extract(1:2, ) %>%
    dplyr::mutate(environment = 1)

  rap$Configuration %<>%
    dplyr::bind_rows(new_environment)
  ###


  ########################################
  # Algorithm
  ########################################
  full_names = c(
    GIL = "Gillespie",
    DET = "Deterministic Waiting Times Algorithm"
  )

  if (semantics %in% names(full_names)) {
    cat("\nUsing the", crayon::bold(full_names[semantics]), "algorithm...")
  } else {
    cat("\nThe custom semantics option is not supported yet, using the Gillespie algorithm...")
    semantics = GIL
  }

  ########################################
  ########################################
  # Applying the algorithm
  ########################################
  ########################################
  rule_id = 1
  membrane_id = 1


  if (semantics == "GIL") {
    ########################################
    # Gillespie algorithm
    ########################################
    simulation_time = 0
    prov_RAP = rap$Configuration

    n_envs = prov_RAP %$%
      environment %>%
      max() %>%
      sum(1) # We start with 0

    # grouped_RAP = prov_RAP %>%
    #   dplyr::group_by(environment)

    gil_exit = tibble::tibble()

    for (i in 0:n_envs) {
      env_i = prov_RAP %>%
        dplyr::filter(environment == i)
      gil_exit %<>%
        dplyr::bind_rows(RAPS::alg_gillespie(env_i)) # (j_c, tau_c)
    }

    gil_exit %<>%
      dplyr::arrange(dplyr::desc(tau_c)) # TODO: Check if desc or not

    # We stop here, as it wouldn't make sense to have the following inside a "choose_rule" function
    # We develop, as a result, the simulate_gil function

  }

  ########################################
  # Returning the generated exit
  ########################################
  cat("\n\tWe choose the rule with id:", crayon::bold(rule_id), "in the membrane with id:", crayon::bold(membrane_id))
  return(c(rule_id, membrane_id))
}
