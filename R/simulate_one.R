#' Simulate one evolutionary step in a given P system
#'
#' TODO: The description.
#' @param rap A rap object.
#' @return A rap object.
#' @examples
#' TODO.
#' @section Warning:
#' This function comes with no warranty.
#' @export
simulate_one = function(rap) {
  cat("\nSimulating one step...")
  return(RAPS::simulate(rap, n_steps = 1))
}
