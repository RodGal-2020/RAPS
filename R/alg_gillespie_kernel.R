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
alg_gillespie_kernel = function(rules) {
  cat(crayon::italic("\n\talg_gillepie_kernel"), "is under develpment")
  ##################
  ##### KERNEL #####
  ##################

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
