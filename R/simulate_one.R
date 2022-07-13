#' Simulate one evolutionary step in a given P system
#'
#' TODO: The description.
#' @param rap A rap object
#' @return A rap object
#' @examples
#' TODO: Some examples
#' @section Warning:
#' This function comes with no warranty.
#' @export
simulate_one = function(rap) {
  cat("What did you expect running a function whose name is ", crayon::italic("template"), "?", sep = "")
  RAPS::simulate(rap, n_steps = 1)
}
