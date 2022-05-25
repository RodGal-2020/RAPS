#' Load a P System given as a XML file
#'
#' This functions admits a demo mode, loading predefined systems. It is the basic function of the package.
#' @param demo_mode Use demo settings? If FALSE path is mandatory.
#' @param path Path to the input file if necessary.
#' @param verbose Verbose?
#' @return A list with a loaded data.frame and the properties of the read system
#' @examples
#' example_p_system = read_xml_p_system()
#' read_xml_p_system(demo = FALSE, path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua/transition_full.xml")
#' @section Warning:
#' Experimental function.
#' @export
read_xml_p_system = function(demo_mode = TRUE, path = NULL, verbose = TRUE) {
  cat("Using RAPS", packageDescription("RAPS", fields = "Version"), "\n\n")


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

  # if(is.null) {exit} else {var}
  nullgo = function(var, ex = NA) {
    if (is.null(var)) {
      return(ex)
    } else {
      return(var)
    }
  }

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
              "Initial_config" = tibble::tibble(),
              "Properties" = tibble::tibble(System = 1))

  # Model selection
  cat("If there is one, which general model does the file follow?\n")
  cases = c("Transition", "Active Membrane", "Custom")
  cat(paste(1:length(cases), ": ", cases, ",", sep = ""), "\nChoose one:")
  case = cases %>% magrittr::extract(readline() %>% as.integer())

  ## TODO
  if (case == "Custom") {
    cat("TODO: Include property file in order to avoid this info and generalize as much as possible\n")
    stop("Not made yet. If you were trying to use the demo mode, try using the transition option.")
  }

  if (demo_mode) {
    cat("Using the", case, "case in demo mode\n") %>% verbose_print
    cat("Choose between transition_i.xml for i in 1:8 or transition_full.xml for full example\n")
    xml_file = readline()
    dir = paste0("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua/", xml_file)
    # dir = switch(case,
    #              "Transition" = "data/transition.xml", # Squares' generator
    #              "Transition demo" = "data/transition_demo.xml", # One w/out meaning but with all the types of trasition rules
    #              "Active Membrane" = "data/activemembranes.xml") # The one solving the SATEl que resuelve el SAT
    cat("Using the following demo directory:", dir, "\n") %>% verbose_print
  } else {
    if(missing(path)) {
      stop("Path required")
    }
    cat("Using the", case, "case\n") %>% verbose_print
    dir = path
    cat("Using the following custom directory:", case, "\n") %>% verbose_print
  }

  data_xml = xml2::read_xml(dir)

  # model type: transition, TODO: complete.
  model_type = data_xml %>%
    xml2::xml_attr("model")

  exit$Properties %<>%
    dplyr::mutate("PLingua_Model_type" = model_type)


  # init_config
  init_config_membranes = data_xml %>%
    xml2::xml_children() %>%
    magrittr::extract(1) %>%
    xml2::xml_find_all('//membrane')

  n_membranes = length(init_config_membranes)
  exit$Properties %<>%
    dplyr::mutate("N_membranes" = n_membranes)

  label = init_config_membranes %>%
    xml2::xml_attr("label")

  ## BEFORE
  # init_config = tibble(label,
  #                      direct_descendants = list(NA),
  #                      objects = list(tibble(names = NA, multiplicity = NA)))
  ## AFTER
  init_config = tibble(label,
                       direct_descendants = list(NA))
  objects = process_multiset(init_config_membranes, label, n_membranes)
  init_config %<>%
    dplyr::left_join(objects)

  for (i in 1:n_membranes) {
    children = init_config_membranes %>%
      magrittr::extract(i) %>%
      xml2::xml_child()

    # Descendants
    init_config$direct_descendants[[i]] = children %>%
      xml2::xml_children() %>%
      xml2::xml_attr("label") %>%
      na_omit() %>%
      nullgo()

    ## BEFORE
    # Objects
    # new_objects = children %>%
    #   xml2::xml_find_all(".//object")
    #
    # object_names = new_objects %>%
    #   xml2::xml_attr("name")
    #
    # multiplicity = new_objects %>%
    #   xml2::xml_attr("multiplicity")
    #
    # m_length = multiplicity %>% length
    # for (j in 1:m_length) {
    #   multiplicity[j] %<>% nago(ex = 1)
    #   # Update with mutate or apply
    #   # multiplicity %>% apply(FUN = nago(ex = 1), MARGIN = 2)
    #   # multiplicity %>% mutate(across(value, nago))
    # }
    #
    #     new_objects = tibble(object_names, multiplicity)
    #
    #     init_config$objects[[i]] = new_objects
  }

  exit$Initial_config = init_config

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

    ## BEFORE
    # new_objects = lhs_nodes %>%
    #   xml2::xml_find_all(".//outer_membrane/multiset/object")
    # # %>%
    # #   xml2::xml_attrs() # Generamos name y multiplicity
    # # TODO: Copy new_objects style as tibbles
    #
    # object_names = new_objects %>%
    #   xml2::xml_attr("name")
    #
    # multiplicity = new_objects %>%
    #   xml2::xml_attr("multiplicity")
    #
    # m_length = multiplicity %>% length
    # for (j in 1:m_length) {
    #   multiplicity[j] %<>% nago(ex = 1)
    # }
    #
    # lhs_multisets = tibble(object_names, multiplicity)
    ## AFTER
    lhs_multisets = process_multiset(xml_multiset = lhs_nodes, label = 1, n_membranes = 1) %>%
      extract("objects") %>%
      extract2(1) %>%
      extract2(1)
    cat("\t\tlhs_multisets: ", print_multiset(lhs_multisets), "\n", sep = "")

    new_row %<>%
      dplyr::mutate(lhs_multisets = list(lhs_multisets))

    lhs_inner_membranes = lhs_nodes %>%
      xml2::xml_find_all(".//inner_rule_membranes/inner_membrane") %>%
      xml2::xml_attr("label")
    ie.empty(lhs_inner_membranes,
             {
               cat("\tlhs_inner_membranes: -\n", sep = "")
             },
             {
               cat("\tlhs_inner_membranes: ", paste0(lhs_inner_membranes %>% unlist, collapse = ", "), "\n", sep = "")
               new_row %<>%
                 dplyr::mutate(lhs_inner_membranes = lhs_inner_membranes)
               ## BEFORE
               # lhs_inner_membranes_multisets = lhs_nodes %>%
               #   xml2::xml_find_all(".//inner_rule_membranes/inner_membrane/multiset/object") %>%
               #   xml2::xml_attrs() # Generamos name y multiplicity
               ## AFTER
               xml_multiset = lhs_nodes %>% xml2::xml_find_all(".//inner_rule_membranes/inner_membrane")
               tryCatch(
                 {
                   process_multiset(xml_multiset, label = 1, n_membranes = 1)
                   lhs_inner_membranes_multisets = process_multiset(xml_multiset, label = 1, n_membranes = 1) %>%
                     extract("objects") %>%
                     extract2(1) %>%
                     extract2(1)
                   cat("\t\tlhs__inner_membranes_multisets: ", print_multiset(lhs_multisets), "\n", sep = "")

                   new_row %<>%
                     dplyr::mutate(lhs_inner_membranes_multisets = list(lhs_inner_membranes_multisets))
                 },
                 error=function(cond) {
                   cat("")
                 },
                 warning=function(cond) {
                   cat("Warning")
                 })
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

  return(exit)
}

