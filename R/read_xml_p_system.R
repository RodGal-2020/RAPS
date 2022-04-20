#' Load a P System given as a XML file
#'
#' This functions admits a demo mode, loading predefined systems. It is the basic function of the package.
#' @param demo_mode Use demo settings? If FALSE path is mandatory.
#' @param path Path to the input file if necessary.
#' @param verbose Verbose?
#' @return A list with a loaded data.frame and the properties of the read system
#' @examples
#' example_p_system = read_xml_p_system()
#' read_xml_p_system(demo = FALSE, path = "data/transition.xml")
#' @export
read_xml_p_system = function(demo_mode = TRUE, path = NULL, verbose = TRUE) {
  ### Quickload of necessary packages:
  # if (!require("pacman")) {install.packages("pacman")} # Paquete para poder hacer lo siguiente
  # pacman::p_load(
  ##   "tidyverse", # Maybe useless
  #   "magrittr",
  #   "dplyr",
  #   "tibble",
  #   "xml2")
  cat("Using RAPS 0.02\n\n")


  ### Aux functions
  # verbose_print = if(verbose){}
  verbose_print = function(action) {
    if (verbose) {
      action
    }
  }

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


  # Exit parameters
  exit = list("Rules" = tibble::tibble(),
              "Initial_config" = NULL,
              "Properties" = tibble::tibble(System = 1))

  # Model selection
  cat("If there is one, which general model does the file follow?\n1: Transition; 2: Active Membrane; 3: Custom (?); 4: Transition demo.\nChoose one: ")
  cases = c("T", "AM", "Custom", "Td")
  case = cases %>% magrittr::extract(readline() %>% as.integer())

  ## TODO
  if (case == "Custom") {
    cat("TODO: Include property file in order to avoid this info and generalize as much as possible\n")
    stop("Not made yet.")
  }

  if (demo_mode) {
    cat("Using the", case, "case\n") %>% verbose_print
    dir = switch(case,
                 "T" = "data/transition.xml", # Squares' generator
                 "Td" = "data/transition_demo.xml", # One w/out meaning but with all the types of trasition rules
                 "AM" = "data/activemembranes.xml") # The one solving the SATEl que resuelve el SAT
    cat("Using the following directory:", dir, "\n") %>% verbose_print
  } else {
    if(missing(path)) {
      cat()
      stop("Path required")
    }
    cat("Using the", case, "case\n") %>% verbose_print
    dir = path
    cat("Using the following directory:", case, "\n") %>% verbose_print
  }

  data_xml = xml2::read_xml(dir)
  # data_list = data_xml %>% xml2::as_list() # TODO: Delete if useless

  # model type: transition
  model_type = data_xml %>%
    xml2::xml_attr("model")

  exit$Properties %<>%
    dplyr::mutate("PLingua_Model_type" = model_type)

  # init_config
  # init_config_xml_nodeset = data_xml %>%
  #   xml2::xml_children() %>%
  #   magrittr::extract(1) %>%
  #   xml2::xml_find_all( '//membrane')

  # init_config paths
  # init_config_paths = init_config_xml_nodeset %>%
  #   xml2::xml_path()

  # rules
  rules_xml_nodeset = data_xml %>%
    xml2::xml_children() %>%
    magrittr::extract(2) %>%
    xml2::xml_find_all( '//rule')

  # rules' paths
  # rules_paths = rules_xml_nodeset %>%
  #   # xml_find_all( '//rule') %>% # Ya busqué arriba
  #   xml2::xml_path()

  n_rules = length(rules_xml_nodeset)
  exit$Properties %<>%
    dplyr::mutate("N_rules" = n_rules)

  if (case == "T") {
    cat(crayon::bold("Reglas:\n"))
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
    new_row = tibble::tibble(rule_id = i)

    cat(crayon::bold("r_", i, "\n", sep= ""))

    rule_i_children = rules_xml_nodeset[i] %>%
      xml2::xml_children()


    ### @d
    dissolves = rules_xml_nodeset[i] %>%
      xml2::xml_attr("dissolves") %>%
      nago()
    cat("\tDissolves? ", dissolves, "\n", sep = "")
    new_row %<>%
      dplyr::mutate(dissolves = dissolves)

    ### Priority
    priority = rules_xml_nodeset[i] %>%
      xml2::xml_attr("priority") %>%
      nago()
    cat("\tPriority: ", priority, "\n", sep = "")
    new_row %<>%
      dplyr::mutate(priority = priority)

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
    new_row %<>%
      dplyr::mutate(lhs_outer_membrane_label = lhs_outer_membrane_label,
                       lhs_outer_membrane_charge = lhs_outer_membrane_charge)

    lhs_multisets = lhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane/multiset/object") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    cat("\t\tlhs_multisets: ", paste0(lhs_multisets %>% unlist, collapse = ", "), "\n", sep = "")
    new_row %<>%
      dplyr::mutate(lhs_multisets = list(lhs_multisets))

    lhs_inner_membranes = lhs_nodes %>%
      xml2::xml_find_all(".//inner_rule_membranes/inner_membrane") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    ie.empty(lhs_inner_membranes,
             {
               cat("\tlhs_inner_membranes: -\n", sep = "")
             },
             {
               cat("\tlhs_inner_membranes: ", paste0(lhs_inner_membranes %>% unlist, collapse = ", "), "\n", sep = "")
               new_row %<>%
                 dplyr::mutate(lhs_inner_membranes = lhs_inner_membranes)
               lhs_inner_membranes_multisets = lhs_nodes %>%
                 xml2::xml_find_all(".//inner_rule_membranes/inner_membrane/multiset/object") %>%
                 xml2::xml_attrs() # Generamos name y multiplicity
               cat("\t\tlhs_inner_membranes_multisets: ", paste0(lhs_inner_membranes_multisets %>% unlist, collapse = ", "), "\n", sep = "")
               new_row %<>%
                 dplyr::mutate(lhs_inner_membranes_multisets = list(lhs_inner_membranes_multisets))
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
    new_row %<>%
      dplyr::mutate(rhs_outer_membrane_label = rhs_outer_membrane_label,
                       rhs_outer_membrane_charge = rhs_outer_membrane_charge)

    rhs_multisets = rhs_nodes %>%
      xml2::xml_find_all(".//outer_membrane/multiset/object") %>%
      xml2::xml_attrs() # Generamos name y multiplicity
    cat("\t\trhs_multisets: ", paste0(rhs_multisets %>% unlist, collapse = ", "), "\n", sep = "")
    new_row %<>%
      dplyr::mutate(rhs_multisets = list(rhs_multisets))

    rhs_outer_membrane_label_2 = rhs_nodes %>%
      xml2::xml_find_all(".//second_outer_membrane") %>%
      xml2::xml_attr("label")
    ie.empty(rhs_outer_membrane_label_2,
             {
               cat("\trhs_outer_membrane_label_2: -\n", sep = "")
             },
             {
               rhs_outer_membrane_charge_2 = rhs_nodes %>%
                 xml2::xml_find_all(".//second_outer_membrane") %>%
                 xml2::xml_attr("charge") %>%
                 nago(ex = "0")
               cat("\trhs_label_2: ", paste(rhs_outer_membrane_label_2, sep = ", "), "\trhs_charge_2: ", rhs_outer_membrane_charge_2, "\n", sep = "")
               new_row %<>%
                 dplyr::mutate(rhs_outer_membrane_label_2 = rhs_outer_membrane_label_2,
                               rhs_outer_membrane_charge_2 = rhs_outer_membrane_charge_2)


               rhs_multisets_2 = rhs_nodes %>%
                 xml2::xml_find_all(".//second_outer_membrane/multiset/object") %>%
                 xml2::xml_attrs() # Generamos name y multiplicity
               cat("\t\trhs_multisets_2: ", paste0(rhs_multisets_2 %>% unlist, collapse = ", "), "\n", sep = "")
               new_row %<>%
                 dplyr::mutate(rhs_multisets_2 = rhs_multisets_2)
               })

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
               new_row %<>%
                 dplyr::mutate(rhs_inner_membranes = rhs_inner_membranes,
                               rhs_inner_membranes_multisets = list(rhs_inner_membranes_multisets))
             })


    exit$Rules %<>%
      dplyr::bind_rows(new_row)
    cat("####################################################\n")
  }

  # Return data
  # colnames(exit$Properties) = "Value" # TODO: Delete if useless
  return(exit)
}
