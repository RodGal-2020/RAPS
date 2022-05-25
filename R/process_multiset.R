#' Process a given multiset given as a XML nodeset
#'
#' This function has not been tested enough yet.
#' @param xml_multiset The xml2 variable with the objects.
#' @param label A vector of labels for the different membranes.
#' @param n_membranes The number of membranes in the considered system.
#' @return A processed multiset, given as a tibble.
#' @examples
#' TODO
#' example_p_system = read_xml_p_system()
#' read_xml_p_system(demo = FALSE, path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua/transition_full.xml")
#' @section Warning:
#' Experimental function.
#' @export
process_multiset = function(xml_multiset, label, n_membranes) {

  multiset = tibble(label,
                    objects = list(tibble(names = NA, multiplicity = NA)))

  for (i in 1:n_membranes) {
    children = xml_multiset %>%
      magrittr::extract(i) %>%
      xml2::xml_child()

    # Objects
    new_objects = children %>%
      xml2::xml_find_all(".//object")

    object_names = new_objects %>%
      xml2::xml_attr("name")

    multiplicity = new_objects %>%
      xml2::xml_attr("multiplicity")

    m_length = multiplicity %>% length
    for (j in 1:m_length) {
      multiplicity[j] %<>% nago(ex = 1)
      # Update with mutate or apply
      # multiplicity %>% apply(FUN = nago(ex = 1), MARGIN = 2)
      # multiplicity %>% mutate(across(value, nago))
    }

    new_objects = tibble(object_names, multiplicity)

    multiset$objects[[i]] = new_objects
  }

  multiset
}
