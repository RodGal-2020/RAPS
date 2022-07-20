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
simulate_gil = function(rap_environment) {
  ### DELETE THIS DEMO
  cat("\nUsing the demo rap...")
  rap = RAPS::path2rap()
  new_environment = rap$RAP %>%
    magrittr::extract(1:2, ) %>%
    dplyr::mutate(environment = 1)

  rap$RAP %<>%
    dplyr::bind_rows(new_environment)
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

  gil_exit %<>%
    dplyr::arrange(dplyr::desc(tau_c)) # TODO: Check if desc or not

  ### DELETE THIS DEMO
  gil_exit[2,] = list(1, 2, 3)
  ###

  chosen_trinity = gil_exit %>%
    magrittr::extract(1, )

  simulation_time %<>%
    sum(chosen_trinity$tau_c)

  gil_exit %>%


}
