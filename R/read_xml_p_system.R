#' Load a P System given as a XML file
#'
#' Bla bla bla
#' @param path Path to the input file
#' @param verbose Verbose?
#' @param print Show read data?
#' @return A loaded data.frame
#' @export
read_xml_p_system = function(path, verbose = TRUE, print = TRUE) {
  ### Quickload of necessary packages:
  # if (!require("pacman")) {install.packages("pacman")} # Paquete para poder hacer lo siguiente
  # pacman::p_load(
  #   "tidyverse",
  #   "magrittr",
  #   "xml2")


  ### Aux functions
  # if(is.na) {exit} else {var}
  nago = function(var, ex = "-") {
    if (is.na(var)) {
      return(ex)
    } else {
      return(var)
    }
  }

  # Examples
  # nago(NA)
  # nago("+1")
  # nago(NA, ex = "0")


  # is.empty?
  is.empty = function(var) {
    return(length(var) == 0)
  }

  # if(empty) {act1} else {act2}
  ie.empty = function(var, act1, act2) {
    if (is.empty(var)) {
      act1
    } else {
      act2
    }
  }

  # Examples
  # is.empty(list())
  # is.empty(list("a"))
  # ie.empty(list(), cat("act1"), cat("act2"))
  # ie.empty(list("a"), cat("act1"), cat("act2"))


  # Missing parameters
  if(missing(path)) {
    cat()
    stop("Path required")
  }

  # Remember avoiding the "library()" function
  cat("Function under development")

  cat("Which general model follows the file?\n1: Transition; 2: Active Membrane; 3: Custom (?).\nChoose one: ")
  cases = c("T", "AM", "Custom")
  case = cases %>% magrittr::extract(readline() %>% as.integer())

  # case = "AM"
  # cat("Escoge una de las siguientes opciones: 1: T; 2: AM\n")


  dir = switch(case,
               "T" = "data/transition.xml", # El generador de cuadrados
               "Td" = "data/transition_demo.xml", # Uno sin sentido pero con todos los tipos de reglas de los sistemas P de transición
               "AM" = "data/activemembranes.xml") # El que resuelve el SAT
  cat("Nos encontramos en el caso", case, "\n")
  data_xml = xml2::read_xml(dir)
  data_list = data_xml %>% xml2::as_list()


  # model type: transition
  model_type = data_xml %>%
    xml2::xml_attr("model")


  # init_config
  init_config_xml_nodeset = data_xml %>%
    xml2::xml_children() %>%
    magrittr::extract(1) %>%
    xml2::xml_find_all( '//membrane')

  # init_config paths
  init_config_paths = init_config_xml_nodeset %>%
    xml2::xml_path()

  # rules
  rules_xml_nodeset = data_xml %>%
    xml2::xml_children() %>%
    magrittr::extract(2) %>%
    xml2::xml_find_all( '//rule')

  # rules' paths
  rules_paths = rules_xml_nodeset %>%
    # xml_find_all( '//rule') %>% # Ya busqué arriba
    xml2::xml_path()

  n_rules = length(rules_xml_nodeset)

  cat(crayon::bold("Reglas:\n"))

  if (case == "AM") {
    cat(
      "\t[a --> @d]'3;\n",
      "\t[b --> b*2]'3;\n",
      "\t[c --> c*3, d*4]'3;\n",
      "\t[d []'4 --> [d]'4]'2;\n",
      "\t[[e]'4 --> e []'4]'2;\n",
      "\t[f [g]'4 --> g [f]'4]'2;\n",
      "(1)\t[h --> h]'2;\n",
      "(2)\t[i --> i]'2;\n")
  }


  cat("\n####################################################\n")
  for (i in 1:n_rules) {
    # Para cada regla

    cat(crayon::bold("r_", i, "\n", sep= ""))

    rule_i_children = rules_xml_nodeset[i] %>%
      xml2::xml_children()


    ### @d
    dissolves = rules_xml_nodeset[i] %>%
      xml2::xml_attr("dissolves") %>%
      nago()
    cat("\tDissolves? ", dissolves, "\n", sep = "")


    ### Priority
    priority = rules_xml_nodeset[i] %>%
      xml2::xml_attr("priority") %>%
      nago()
    cat("\tPriority: ", priority, "\n", sep = "")

    ### LHS
    cat(crayon::green("\nLHS\n"))
    lhs_nodes = rule_i_children %>% magrittr::extract(1) # Solo lhs

    lhs_outer_membrane_label = lhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane") %>%
      xml2::xml_attr("label")
    lhs_outer_membrane_charge = lhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane") %>%
      xml2::xml_attr("charge") %>%
      nago(ex = "0")
    cat("\tlhs_label: ", paste(lhs_outer_membrane_label, sep = ", "), "\tlhs_charge: ", lhs_outer_membrane_charge, "\n", sep = "")

    lhs_multisets = lhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane/multiset/object") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    cat("\t\tlhs_multisets: ", paste0(lhs_multisets %>% unlist, collapse = ", "), "\n", sep = "")

    lhs_inner_membranes = lhs_nodes %>%
      xml2::xml_find_all(".//inner_rule_membranes/inner_membrane") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    ie.empty(lhs_inner_membranes,
             cat("\tlhs_inner_membranes: -\n", sep = ""),
             {
               cat("\tlhs_inner_membranes: ", paste0(lhs_inner_membranes %>% unlist, collapse = ", "), "\n", sep = "")
               lhs_inner_membranes_multisets = lhs_nodes %>%
                 xml2::xml_find_all(".//inner_rule_membranes/inner_membrane/multiset/object") %>%
                 xml2::xml_attrs() # Generamos name y multiplicity
               cat("\t\tlhs_inner_membranes_multisets: ", paste0(lhs_inner_membranes_multisets %>% unlist, collapse = ", "), "\n", sep = "")
             })


    ### RHS
    cat(crayon::magenta("\nRHS\n"))
    rhs_nodes = rule_i_children %>% magrittr::extract(2) # Solo rhs

    rhs_outer_membrane_label = rhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane") %>%
      xml2::xml_attr("label")
    rhs_outer_membrane_charge = rhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane") %>%
      xml2::xml_attr("charge") %>%
      nago(ex = "0")
    cat("\trhs_label: ", paste(rhs_outer_membrane_label, sep = ", "), "\trhs_charge: ", rhs_outer_membrane_charge, "\n", sep = "")

    rhs_multisets = rhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane/multiset/object") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    cat("\t\trhs_multisets: ", paste0(rhs_multisets %>% unlist, collapse = ", "), "\n", sep = "")

    rhs_outer_membrane_label_2 = rhs_nodes %>%
      xml2::xml_find_all(".//second_outer_membrane") %>%
      xml2::xml_attr("label")
    rhs_outer_membrane_charge_2 = rhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane") %>%
      xml2::xml_attr("charge") %>%
      nago(ex = "0")
    cat("\trhs_label_2: ", paste(rhs_outer_membrane_label_2, sep = ", "), "\trhs_charge_2: ", rhs_outer_membrane_charge_2, "\n", sep = "")

    rhs_multisets_2 = rhs_nodes %>%
      xml2::xml_find_all(".//second_outer_membrane/multiset/object") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    cat("\t\trhs_multisets_2: ", paste0(rhs_multisets_2 %>% unlist, collapse = ", "), "\n", sep = "")

    rhs_inner_membranes = rhs_nodes %>%
      xml2::xml_find_all(".//inner_rule_membranes/inner_membrane") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    ie.empty(rhs_inner_membranes,
             cat("\trhs_inner_membranes: -\n", sep = ""),
             {
               cat("\trhs_inner_membranes: ", paste0(rhs_inner_membranes %>% unlist, collapse = ", "), "\n", sep = "")
               rhs_inner_membranes_multisets = rhs_nodes %>%
                 xml2::xml_find_all(".//inner_rule_membranes/inner_membrane/multiset/object") %>%
                 xml2::xml_attrs() # Generamos name y multiplicity
               cat("\t\trhs_inner_membranes_multisets: ", paste0(rhs_inner_membranes_multisets %>% unlist, collapse = ", "), "\n", sep = "")
             })


    cat("####################################################\n")
  }
}

## Error
# read_xml_p_system()
