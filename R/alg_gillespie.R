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
alg_gillespie = function(rap) {
  cat(crayon::italic("\n\talg_gillepie"), "is under develpment, returning", crayon::italic("(j, tau) = (0,1)"), "by default")

  ### DELETE THIS DEMO
  cat("\nUsing the demo rap...")
  rap = RAPS::path2rap()
  ###

  rules = rap$Rules

  exit_rule = alg_gillespie_kernel(rules) # Written as an independent function for clearness

  return(exit_rule)
}
