#' Kernel of Gillespie's algorithm
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Kernel of Gillespie's algorithm, required for both multienvironmental and monoenvironmental simulations.
#'
#' @param rules A set of rules
#'
#' @return
#' A rule to be executed, given as a `tibble` object with fields `j_c`, the rule id, and `tau_c`, the execution time of the rule.
#'
#' @section TODO:
#' Add references.
#'
#' @export
alg_gillespie_kernel = function(rap) {
  # cat(crayon::italic("\n\talg_gillepie_kernel"), "is under develpment")

  ##################
  ##### KERNEL #####
  ##################

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  # library(RAPS)
  # rap = RAPS::load_demo_dataset("FAS")
  ###

  p = rap$Rules$propensity
  n_rules = dim(rap$Rules)[1]

  p_0 = sum(p)

  a_1 = runif(1)
  a_2 = runif(1)

  # Selected rule
  i_0 = sample(x = 1:n_rules, size = 1, prob = p / p_0) # Equivalent to choosing the index verifying the condition

  # Waiting time
  tau = 1 / p_0 * log(1 / a_1)

  exit_rule = tibble::tibble(i = i_0, tau = tau)

  return(exit_rule)
}
