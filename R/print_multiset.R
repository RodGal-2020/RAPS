#' Improved representation of a processed multiset
#'
#' This function has not been tested enough yet.
#' @param multiset The processed multiset.
#' @return Nothing, it only prints.
#' @examples
#' TODO
#' example_p_system = read_xml_p_system()
#' read_xml_p_system(demo = FALSE, path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua/transition_full.xml")
#' @section Warning:
#' Experimental function.
#' @export
print_multiset = function(multiset) {
  # Example for tests
  # multiset = tibble(object_names = c("a", "b", "c"), multiplicity = 1:3)
  paste(multiset$object_names, multiset$multiplicity, sep = "*", collapse = ", ")
}
