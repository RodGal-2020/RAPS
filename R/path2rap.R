#' Load a P System given as a path/URL to a XML/JSON file
#'
#' This is the basic function of the package, and allows us to read a given P system, turning it into a rap object.
#' `r lifecycle::badge("experimental")`
#' @param path Path/URL to the input file.
#' @param verbose Level of verbosity, between 0 and 5.
#' @param demo To execute as a demo.
#' @param max_depth To look for new structures in the jerarchy of the P system.
#' @return A rap object.
#' @examples
#' TODO.
#' @section Warning:
#' Experimental function.
#' @section Future work:
#' - Include different formats for inputs, like JSON or even the `.pli` itself.
#' @export
path2rap = function(path, use_codification = FALSE, verbose = 5, demo = FALSE, debug = FALSE) {
  cat("Using RAPS", packageDescription("RAPS", fields = "Version"), "\n\n")

  ####################################################
  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  ######## Common
  # library(RAPS)
  # use_codification = FALSE
  # verbose = 5
  # demo = FALSE
  # debug = TRUE
  # rap_reference = RAPS::load_demo_dataset("FAS")
  # cat(crayon::bold("CAUTION:", "USING DEMO MODE"))
  ####################################################
  ######## Debugging/Demo files
  #### Evolution rules
  ## Complex evolution rules
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/stochastic_model_001_RAPS_like_evolution.xml"
  ## 0 - a to b
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/0%20-%20%20a_to_b.xml"
  ## 1 - a to b2
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/1%20-%20%20a_to_b2.xml"
  ## 2 - a2 to b3
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/2%20-%20%20a2_to_b3.xml"
  ## 3 - a1,b2 to c3,d4
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/3%20-%20%20a1%2Cb2_to_c3%2Cd4.xml"

  #### Communication rules
  ## 0 - Outside to inside
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/0%20-%20a_outside_to_a_inside.xml"
  ## 1 - Inside to outside
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/1%20-%20a_inside_to_a_outside.xml"
  ## 2 - Multiinsde to multioutside
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/2%20-%20multi_inside_to_multi_outside.xml"

  ## N - Crazy multicommunication
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_communication/N%20-%20crazy_multi_inside_to_crazy_multi_outside.xml"

  #### Stochastic rules
  ## 0 - Stochastic a to b
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_evolution/0%20-%20a_to_b_stochastic.xml"
  ## 0 - Mixed stochastic a to b
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules_evolution/0%20-%20a_to_b_stochastic_mixed.xml"

  if (demo) {
    cat("\nChoose a dataset from the following:\n")
    datasets = c("EGFR", "FAS", "QUORUM", "TEMPLATE", "demo-1", "demo-2", "demo-small")
    cat(paste(datasets, collapse = ", "))
    chosen_dataset = readline()
    return(RAPS::load_demo_dataset(dataset = chosen_dataset))
  }

  ######################################
  ### Aux functions
  ######################################

  ### verbose_print = if(verbose){}
  verbose_print = function(action, minimum_verbose_to_print = 1) {
    if (verbose >= minimum_verbose_to_print) {
      action
    }
  }
  ## Examples
  # cat(verbose)
  # verbose_print(cat("Hola!"))
  # verbose_print(cat("Hola!"), 2)


  ### is.empty?
  is_empty = function(var) {
    return(length(var) == 0 || is.null(var)|| length(var) == 0 || var == "")
  }
  ## Examples
  # is_empty(NA)
  # is_empty(list())
  # is_empty(NULL)
  # is_empty(tibble())


  ### if(is.na or is.null) {exit} else {var}
  ## TODO: Substitute with tidyr::replace_na(var, new_element = "-"), which is compatible w/ mutate & friends
  substitute_if_empty = function(var, new_element = "-") {
    if (is.na(var) || is_empty(var)) {
      return(new_element)
    } else {
      return(var)
    }
  }
  ## Examples
  # substitute_if_empty(NA)
  # substitute_if_empty("+1")
  # substitute_if_empty(NA, new_element = "0")


  ######################################
  # Exit parameters
  ######################################
  exit = list(
    "Configuration" = tibble::tibble(),
    # "Initial_config" = tibble::tibble(), # Included in Configuration
    "Rules" = tibble::tibble(),
    "Properties" = tibble::tibble(System = 1, Note = "System ID included for generalisation"))


  ######################################
  # Basic data reading
  ######################################
  # if (demo_mode) {
  #   cat("Using the demo mode with XML files\n") %>% verbose_print()
  #   cat("Choose between transition_i for i in 1:3u7:8\n")
  #   xml_file = readline()
  #   dir = paste0("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/", xml_file, ".xml")
  #   psystem_pli = paste0("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/", xml_file, ".pli")
  #
  #   psystem_pli %<>% readr::read_lines(n_max = 100)
  #   cat("Codification of the P system in .pli format:")
  #   cat("-----------------------------------------------------------\n")
  #   cat(psystem_pli, sep = "\n")
  #   cat("-----------------------------------------------------------\n")
  #
  #   cat("Using the following demo directory:", dir, "\n") %>% verbose_print
  # } else {
  if(missing(path)) {
    stop("Path required")
  }
  # cat("Using the following directory:", case, "\n") %>% verbose_print
  # }

  data_xml = xml2::read_xml(path) %>%
    xml2::xml_children() %>%
    xml2::xml_children() # Omitting cereal>file

  ######################################
  # Handout of the xml structure
  # cereal > file
  #               > header $Properties
  #               > version $Properties
  #               > psystem
  #                         > objects $Properties
  #                         > labels $RAP (bound to structure)
  #                         > features (?)
  #                         > strings $Properties
  #                         > max_multiplicity $Properties
  #                         > model > id $Properties
  #                         > semantics $RAP
  #                         > structure $RAP (bound to labels)
  #                         > multisets $RAP
  #                         > rules > left_hand_rule, arrow, right_hand_rule $Rules
  #                         > features (again) $Properties
  ######################################


  ##############################################################################
  ##############################################################################
  ################################ Properties ##################################
  ##############################################################################
  ##############################################################################
  properties = list()

  ## Property: PLingua output version
  properties$output_version = data_xml %>%
    xml2::xml_find_all('//version') %>%
    xml2::xml_text()

  # Keep only the psystem info
  data_xml %<>%
    magrittr::extract(3) %>% # psystem node
    xml2::xml_children()

  ## Property: Dictionary of rules and real names of objects used in the PS
  objects_node_values = data_xml %>%
    xml2::xml_find_all("//objects") %>%
    xml2::xml_children()

  properties$objects_dictionary = tibble::tibble(
    real_name = objects_node_values %>%
      xml2::xml_text(),
    codification_as_id = objects_node_values %>%
      xml2::xml_name() %>%
      substr(start = 6, stop = 8) # Only 3 digits

  )

  if (!use_codification) {
    objects_dictionary = properties$objects_dictionary
    colnames(objects_dictionary) = c("true_object", "object")

    translate_objects = function(object_tibble) {
      return(object_tibble %>%
        dplyr::inner_join(objects_dictionary, by = "object") %>%
        dplyr::select(true_object, multiplicity) %>%
        # dplyr::select(-object) %>%
        dplyr::rename(object = true_object)
        )
    }
  }

  ## Property: Labels used in the membrane structure
  labels_node = data_xml %>%
    xml2::xml_find_all("//labels")

  codification = labels_node %>%
    xml2::xml_children() %>%
    xml2::xml_name()

  real_value = labels_node %>%
    xml2::xml_children() %>%
    xml2::xml_text() # May not be numbers

  properties$labels_dictionary = tibble::tibble(codification, real_value)

  ## Property: Features
  properties$features = data_xml[3] %>%
    xml2::xml_children() %>%
    xml2::xml_text() # pattern, sc (for stochastic constant)

  ## Property: Strings
  properties$strings_or_model_rules_names = data_xml %>%
    xml2::xml_find_all("//strings") %>%
    xml2::xml_children() %>%
    xml2::xml_text()

  ## Property: Max multiplicity
  properties$max_multiplicity = data_xml %>%
    xml2::xml_find_all("//max_multiplicity") %>%
    xml2::xml_integer()

  ## Property: Model id
  properties$model_id = data_xml %>%
    xml2::xml_find_all("//model") %>%
    xml2::xml_text()

  ## TODO: Property: Semantics (rule type codification as id [just like the objects_dictionary])
  properties$semantics = NA
  # semantics = list()
  # semantics_node_children = data_xml %>%
  #   xml2::xml_find_all("//semantics") %>%
  #   xml2::xml_children()
  # semantics$value = semantics_aux %>% magrittr::extract(1) %>% xml2::xml_integer()
  # semantics$inf = semantics_aux %>% magrittr::extract(2) %>% xml2::xml_text()
  # if (semantics$inf == "true") {
  #   semantics$inf = TRUE
  # } else {
  #   semantics$inf = FALSE
  # }
  # semantics$patterns = "TODO"
  # semantics$children_size = semantics_aux %>% magrittr::extract(4) %>% xml2::xml_attr("size")

  # properties$semantics = semantics
  # verbose_print(cat("\n", crayon::bold("Semantics"), " (rule information) not supported yet", sep = ""))

  ######################################
  exit$Properties = properties
  ######################################



  ##############################################################################
  ##############################################################################
  ################################ Configuration ###############################
  ##############################################################################
  ##############################################################################

  ##############################################################################
  ## structure
  ##############################################################################

  structure_node = data_xml %>%
    xml2::xml_find_all("//structure")

  ## Handout of the structure node
  # structure > charge, label
  #           > children
  #                      > charge, label
  #                      > children

  ##############################################################################
  get_name_from_value = function(value) {
    return(value %>%
      xml2::xml_children() %>%
      magrittr::extract(2) %>%
      xml2::xml_children() %>%
      xml2::xml_text()
    )
  }
  ##############################################################################

  ##############################################################################
  get_children_names_from_values = function(nodes) {
    l_nodes = length(nodes)

    children_names = c()

    for (child in 1:l_nodes) {
      # son = children[child] %>%
      #   xml2::xml_children() %>%
      #   magrittr::extract(2) %>%
      #   xml2::xml_text()
      children_names %<>% c(
        get_name_from_value(nodes[child])
        )
    }

    children_names %<>% list()

    return(children_names)
  }
  ##############################################################################

  ##############################################################################
  get_children_from_value = function(node) {
    return(node %>%
      xml2::xml_children() %>%
      magrittr::extract(3) %>%
      xml2::xml_children())
  }
  ##############################################################################

  cat("\nAssuming that only a root/skin node exists")
  skin_id = structure_node %>%
    get_name_from_value()

  children_value = structure_node %>%
    get_children_from_value()

  if (is_empty(children_value)) {
    my_subM = NA
  } else {
    children_names = get_children_names_from_values(children_value)
    my_subM = children_names
  }

  configuration = tibble::tibble(
    id = skin_id,
    subM = my_subM,
    superM = list(NULL)
  )

  while (!is_empty(children_value)) {
    l_children = length(children_value)

    aux_children_value = children_value[1] # Only the first, to initialize the variable
    new_element = 2

    for (child in 1:l_children) {
      child_value = children_value[child]

      new_id = child_value %>%
        get_name_from_value()

      child_children_value = child_value %>%
        get_children_from_value() # FIXME: With child = 2
      # structure_node %>%
      #   xml2::xml_children() %>%
      #   magrittr::extract(3) %>%
      #   xml2::xml_children() %>%
      #   magrittr::extract(2) %>%
      #   xml2::xml_children() %>%
      #   magrittr::extract(3) %>%
      #   xml2::xml_children()

      if (is_empty(child_children_value)) {
        new_subM = NA
      } else {
        # If it has children we had them to the list
        children_names = get_children_names_from_values(child_children_value)
        new_subM = children_names

        n_children = length(child_children_value)

        if (n_children > 0) {
          # aux_aux_children_value = children_value[1] # Only the first, to initialize the variable

          for (i in 1:n_children) {
            aux_children_value[new_element] = child_children_value[i]
            new_element %<>% sum(1)
          }
        }
      }

      new_row = tibble::tibble(
        id = new_id,
        subM = new_subM,
        superM = list(NULL) # We will include those later
      )

      configuration %<>%
        dplyr::bind_rows(new_row)
    }
    children_value = aux_children_value[-1]
  }

  ### Update superM
  # configuration_failsafe = configuration
  # configuration = configuration_failsafe
  for (mem_id in configuration$id) {
    subM = configuration %>%
      dplyr::filter(id == mem_id) %$%
      subM

    if (!is_empty(subM)) {
      for (subM_i in subM[[1]]) {
        aux_configuration = configuration %>%
          dplyr::filter(id == subM_i)

        aux_configuration %<>%
          dplyr::mutate(superM =
                          ifelse(is_empty(superM[[1]]),
                                 list(subM_i),
                                 list(superM[[1]], subM_i))) # FIXME: With append()

        configuration %<>%
          dplyr::filter(id != subM_i) %>%
          dplyr::bind_rows(aux_configuration)
      }
    }
  }

  ## We consider only one parent for each one
  only_one_parent = TRUE
  if (only_one_parent) {
    configuration %<>%
      tidyr::unnest_longer(superM)
  }

  ##############################################################################
  ## multisets
  ##############################################################################
  initial_values = data_xml %>%
    xml2::xml_find_all("//multisets") %>%
    xml2::xml_children()

  multisets_aux = tibble::tibble(
    id = configuration$id,
    objects = list(tibble::tibble(
      object = "@filler",
      multiplicity = 1
    ))
  )

  ##############################################################################
  get_objects_from_multiset = function(multiset, rhs = FALSE, get_mem = FALSE) {
    ## TODO: Delete rhs and get_mem if useless (which is likely)

    # Input: nodeset of valuei, each with a branch with id and, optionally, multiplicity

    ### Options
    ## multiset > values > (key>id), (value>multiplicity)
    ## multiset > #

    ### Debugging rules 2.0:

    # rule_id = 2
    # (multiset = rules_value[rule_id] %>%
    #   ## RHS
    #    # xml2::xml_find_all(".//right_hand_rule") %>%
    #   ## LHS
    #    xml2::xml_find_all(".//left_hand_rule") %>%
    #    xml2::xml_find_first(".//multiset"))

    ### Debugging rules 2.0:

    # LHS - Debugging - Checked
    # cat("Debugging LHS")
    # multiset = lhs_info %>%
    #   xml2::xml_find_all(".//multiset")
    # rhs = FALSE

    # RHS - Debugging
    # cat("Debugging RHS")
    # multiset = rhs_info %>% # multiset, membranes
    #   xml2::xml_find_all(".//membranes") # value0, value1
    # rhs = TRUE


    values = multiset %>%
      xml2::xml_children()

#     if (rhs) {
#       values %>% xml2::xml_children()
#     }

    n_children = length(values) # > 1 by definition of the valuei fields

    object = c()
    multiplicity = c()

    for (child in 1:n_children) {
      chosen_child = values[child]
      aux_children = chosen_child %>%
        xml2::xml_children()

      if (!rhs) {
        new_object = chosen_child %>%
          xml2::xml_find_all(".//id") %>%
          xml2::xml_text() # Can be more than one

        new_multiplicity = chosen_child %>%
          xml2::xml_find_all(".//multiplicity") %>%
          xml2::xml_text() # Can be more than one

      } else {
        mem_id = chosen_child %>%
          xml2::xml_find_all(".//label") %>%
          xml2::xml_find_all(".//id") %>%
          xml2::xml_text() # TODO: Use me for in-out

        chosen_child_multiset = chosen_child %>%
          xml2::xml_find_all(".//multiset")

        new_object = chosen_child_multiset %>%
          xml2::xml_find_all(".//id") %>%
          xml2::xml_text()

        new_multiplicity = chosen_child_multiset %>%
          xml2::xml_find_all(".//multiplicity") %>%
          xml2::xml_text()

      }

      object %<>% c(new_object)
      multiplicity %<>% c(new_multiplicity)
    }

    if (max(length(object), length(multiplicity)) == 0) {
      cat("\nObjects not found")
      return(NA)
    }

    if (rhs & get_mem) {
      return(list(objects = tibble::tibble(object, multiplicity), mem = mem_id))
    } else {
      return(tibble::tibble(object, multiplicity))
    }
  }
  ##############################################################################

  n_values = length(initial_values)

  for (value in 1:n_values) {
    initial_values_children = initial_values[value] %>%
      xml2::xml_children()

    mem_id = initial_values_children[1] %>%
      xml2::xml_text()

    value_node = initial_values_children[2]

    old_multisets_aux = multisets_aux

    multisets_aux %<>%
      dplyr::filter(id == mem_id)

    objects_from_multiset = get_objects_from_multiset(value_node)

    if (!use_codification) {
      objects_from_multiset %<>% translate_objects()
    }

    multisets_aux$objects[[1]] = objects_from_multiset

    multisets_aux = old_multisets_aux %>%
      dplyr::filter(id != mem_id) %>%
      dplyr::bind_rows(multisets_aux)

    # For the following steps
    # ! Perhaps there are not
    # value_children = value_node %>%
    #   xml2::xml_children()
  }

  configuration %<>%
    dplyr::left_join(multisets_aux, by = "id")

  ######################################
  # labels
  ######################################
  verbose_print(cat(crayon::bold("\nlabels"), "are not supported yet, using ids"), 2)
  configuration$labels = configuration$id

  ######################################
  # charge
  ######################################
  verbose_print(cat(crayon::bold("\ncharge"), "is not supported yet"), 2)
  configuration$charge = "-"

  ######################################
  # other_params
  ######################################
  verbose_print(cat(crayon::bold("\nother_params"), "are not supported yet"), 2)
  configuration$other_params = NA

  ######################################
  exit$Configuration = configuration
  ######################################


  ##############################################################################
  ##############################################################################
  ################################ Rules #######################################
  ##############################################################################
  ##############################################################################

  ##############################################################################
  get_membrane_info = function(membrane) {
    ### Returns a tibble with info about the membrane and its inner objects (children are ignored for now)

    ### Options
    ## membrane > charge, label, multiset, children # multiset as before
    ## Looks like it's never empty

    ## Debugging:
    ## LHS
    # rule_id = 1
    # (membrane = rules_value[rule_id] %>%
    #   xml2::xml_find_all(".//left_hand_rule") %>%
    #   xml2::xml_find_first(".//membrane"))

    ## RHS
    # rule_id = 1
    # (membrane = rules_value[rule_id] %>%
    #   xml2::xml_find_all(".//right_hand_rule") %>%
    #   xml2::xml_find_first(".//membranes"))

    ### Charge
    charge = membrane %>%
      xml2::xml_find_all(".//charge") %>%
      xml2::xml_text() %>%
      substitute_if_empty()

    ### Label
    label = membrane %>%
      xml2::xml_find_all(".//label") %>%
      xml2::xml_text()

    ### Multiset
    multiset = membrane %>%
      xml2::xml_find_first(".//multiset") %>%
      preprocess_multiset()

    ### Children
    children = membrane %>%
      xml2::xml_find_all(".//children") %>%
      xml2::xml_text()
    if (!is_empty(children)) {
      verbose_print(cat(crayon::bold("children"), "parameter is not supported yet, which means that we can't work with (membranes inside of)*2 membranes"), 4)
    }

    membrane_info = tibble::tibble(
      charge,
      label,
      children,
      objects = list(multiset)
    )

    return(membrane_info)

  }
  ##############################################################################

  ##############################################################################
  get_rule_from_value = function(value) {
    ## Debugging:
    # (value = rules_value[1])

    new_rule_id = xml2::xml_name(value) # TODO: Use rules dictionary

    lhs_node = value %>% xml2::xml_find_all(".//left_hand_rule")
    rhs_node = value %>% xml2::xml_find_all(".//right_hand_rule")
    features_node = value %>% xml2::xml_find_all(".//features")

    ###############################################
    # lhs
    # In the demo the first rules are: [a -> b]'1, [a -> b*2]'1
    # In "a []'1 -> whatever" a would be in the multiset node, while []'1 would appear in the membrane zone

    ### Multiset
    lhs_multiset_objects = lhs_node %>%
      xml2::xml_find_first(".//multiset") %>% # Objects outside the AM
      get_objects_from_multiset()

    ### Membrane
    lhs_membrane_info = lhs_node %>%
      xml2::xml_find_first(".//membrane") %>% # charge, label, multiset, children
      get_membrane_info()

    if (lhs_membrane_info$charge %>% substitute_if_empty() != "-") {
      verbose_print(cat(crayon::bold("charge"), "in a rule is not supported yet"), 2)
    }

    if (!is_empty(lhs_membrane_info$children)) {
      verbose_print(cat(crayon::bold("children"), "parameter is not supported yet"), 4)
    }

    ###############################################
    # rhs
    # In the demo the first rules are: [a -> b]'1, [a -> b*2]'1
    # In "-> a []'1 " a would be in the multiset node, while []'1 would appear in the membranes zone

    ### Multiset
    rhs_multiset_objects = rhs_node %>%
      xml2::xml_find_first(".//multiset") %>% # Objects outside the AM
      get_objects_from_multiset()

    ### Membrane
    rhs_membrane_info = rhs_node %>%
      xml2::xml_find_first(".//membranes") %>% # Note the "s"
      get_membrane_info()

    if (rhs_membrane_info$charge %>% substitute_if_empty() != "-") {
      verbose_print(cat(crayon::bold("charge"), "in a rule is not supported yet"), 2)
    }

    if (!is_empty(rhs_membrane_info$children)) {
      verbose_print(cat(crayon::bold("children"), "parameter is not supported yet"), 4)
    }

    ##############################################
    ##############################################
    ##############################################
    ##############################################
    ##############################################


    #########################
    #### OLD RHS
    #########################
    rhs_info = rhs_node # multiset, membranes (note the "s")

    ## membrane_label
    new_rhs_membrane_label = new_lhs_membrane_label

    ## multiset
    new_rhs_objects = rhs_info %>%
      get_objects_from_multiset(rhs = TRUE, get_mem = TRUE) %>% # get_mem for in-out rules
      magrittr::extract2("objects")

    ## children
    new_lhs_children = lhs_info %>%
      xml2::xml_find_all("children") %>%
      xml2::xml_text()

    if (!is_empty(new_lhs_children)) {
      verbose_print(cat(crayon::bold("children"), "parameter is not supported yet"), 4)
    }

    ###############################################
    # Parameters for RAPS

    ## where

    ## main_membrane_label
    main_membrane_label = NA
    verbose_print(cat(crayon::bold("membrane_label"), "parameter is under development"), 2)


    ###############################################
    # TODO: features
    # features_node

    ###############################################
    # Stochastic constant # Quick deployment, improvable
    features_children = features_node %>%
      xml2::xml_children()

    if (length(features_children) == 2) {
      new_sc = features_children %>%
        magrittr::extract(2) %>%
        xml2::xml_children() %>%
        magrittr::extract(2) %>%
        xml2::xml_children() %>%
        magrittr::extract(2) %>%
        xml2::xml_text() %>%
        utf8ToInt()
    } else {
      cat("This function only supports sc for now as features.")
      new_sc = NA # TODO: Check this
    }

    ## Update names if necessary
    if (!use_codification) {
      new_lhs_objects %<>% translate_objects()
      new_rhs_objects %<>% translate_objects()
    }

    new_tibble = tibble::tibble(
      rule_id = new_rule_id,
      main_membrane_label = main_membrane_label,
      lhs = list(new_lhs_objects),
      rhs = list(new_rhs_objects),
      propensity = new_sc,
      dissolves = "TODO",
      priority = "TODO",
    )

    return(new_tibble)
  }
  ##############################################################################

  rules_value = data_xml %>%
    xml2::xml_find_all("//rules") %>%
    xml2::xml_children()

  rules = tibble::tibble(
    rule_id = NULL,
    dissolves = NULL,
    priority = NULL,
    main_membrane_label = NULL,
    lhs = NULL,
    rhs = NULL,
    propensity = NULL
  )

  n_rules = length(rules_value)

  ####################################
  exit$Properties$n_rules = n_rules
  ####################################

  for (i in 1:n_rules) {
    rules %<>%
      dplyr::bind_rows(get_rule_from_value(rules_value[i]))
  }

  exit$Rules = rules

  return(exit)
}
