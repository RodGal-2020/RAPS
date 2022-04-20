#' Simulate the evolution of a loaded P System
#'
#' Given a P System and some semantics we want to follow, simulate the evolution of the system.
#' @param p_system The p_system whose evolution we want to simulate.
#' @param verbose Verbose?
#' @param save Do you want to save the results? TODO
#' @param semantic_dir Directory in which the semantics of the system are written. TODO
#' @return A list with a loaded data.frame and the properties of the read system
#' @examples
#' example_p_system = read_xml_p_system()
#' simulate_p_system(example_p_system)
#' @section Warning:
#' Function under developmente.
#' @seealso
#' [read_xml_p_system()] for loading P Systems
#' @export
simulate_p_system = function(p_system, verbose = TRUE, save = FALSE, semantic_dir = "semantics/transition_semantics_1.txt") {
  ### Aux functions
  # verbose_print = if(verbose){}
  verbose_print = function(action) {
    if (verbose) {
      action
    }
  }

  ### Missing parameters
  # TODO: Delete if useless
  # if(missing(p_system)) {
  #   cat()
  #   stop("P System required")
  # }

  cat("Using RAPS 0.03 - read and simulate\n\n")
  if (save) {
    cat("Write the saving directory:\n")
    dir = readline()
    cat("Saving simulation in given directory:", dir, "\n")
    cat("TODO\n")
  }
}

## Demo
# salida = read_xml_p_system()
