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
alg_gillespie = function(rap_environment) {
  cat("What did you expect running a function whose name is ", crayon::italic("template"), "?", sep = "")

  cat(crayon::italic("\n\talg_gillepie"), "is under develpment, returning", crayon::italic("(j, tau) = (0,1)"), "by default")

  j = 0 # Selected rule
  tau = 1 # Waiting time

  exit_rule = tibble::tibble(j_c = j, tau_c = tau)

  return(exit_rule)
}
