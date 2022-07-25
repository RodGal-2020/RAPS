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
alg_det_pdp = function(rap_environment, max_T = 10) {
  cat(crayon::bold("alg_det_pdp() is under development"))

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  cat("\nUsing the demo rap...")
  rap_environment = RAPS::path2rap(demo = 2)
  verbose = 1
  debug = TRUE
  new_environment = rap_environment$RAP %>%
    dplyr::mutate(environment = 1)
  rap_environment$RAP %<>%
    dplyr::bind_rows(new_environment)
  rule_id = 1 # To track errors
  cat("\nWorking with 2 environments, 0 and 1, with the same objects")
  ###


  ########################################
  # Gillespie algorithm
  ########################################
  simulation_time = 0
  n_envs = length(unique(rap_environment$RAP$environment)) - 1 # Instead of max in order to generalize
  rules = rap_environment$Rules
  n_rules = dim(rules)[1]
  propensities = rules$propensity

  cat("\nUsing concentration_of_reactives = 1:n_rules")
  concentration_of_reactives = 1:n_rules # It depends on the environment (!)

  trinities = list()
  for (env in 0:n_envs) {
    # Initialization
    trinities[[env+1]] = tibble::tibble(i = NULL,
                                      tau_i = NULL,
                                      c = NULL)

    for (rule in 1:n_rules) {
      v_r = propensities[rule] * concentration_of_reactives[rule]

      trinities[[env+1]] %<>% dplyr::bind_rows(
          tibble::tibble(
            i = rule,
            tau_i = 1 / v_r,
            c = env
        )
      )
    }

    trinities[[env+1]] %<>%
      dplyr::arrange(tau_i) # Increasing ordder
  }


  ########################
  ##### END FUNCTION #####
  ########################
  return(rap)
}
