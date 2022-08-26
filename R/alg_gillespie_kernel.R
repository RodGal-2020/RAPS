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
alg_gillespie_kernel = function(rules) {
  cat(crayon::italic("\n\talg_gillepie_kernel"), "is under develpment")
  ##################
  ##### KERNEL #####
  ##################

  ### DELETE THIS DEMO
  # cat("\nUsing the demo rap...")
  # rap = RAPS::path2rap(demo = 2)
  # rules = rap$Rules
  ###

  p = rules$propensity
  n_rules = dim(rules)[1]

  p_0 = sum(p)

  a_1 = runif(1)
  a_2 = runif(1)

  # Selected rule
  j_0 = sample(x = 1:n_rules, size = 1, prob = p / p_0) # Equivalent to choosing the index verifying the condition

  # Waiting time
  tau = 1 / p_0 * log(1 / a_1)

  exit_rule = tibble::tibble(j_c = j_0, tau_c = tau)

  return(exit_rule)
}
