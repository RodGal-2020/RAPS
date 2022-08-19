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
path2rap = function(path = NULL, verbose = 5, demo = 1, debug = FALSE) {
  cat("Using RAPS", packageDescription("RAPS", fields = "Version"), "\n\n")


  ####################################################
  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  # library(RAPS)
  ## Complex evolution rules
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/stochastic_model_001_RAPS_like_evolution.xml"
  ## 0 - a to b
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules/0%20-%20%20a_to_b.xml"
  ## 1 - a to b2
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules/1%20-%20%20a_to_b2.xml"
  ## 2 - a2 to b3
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules/2%20-%20%20a2_to_b3.xml"
  ## 3 - a1,b2 to c3,d4
  # path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/increasing_rules/3%20-%20%20a1%2Cb2_to_c3%2Cd4.xml"

  ######## Common
  # demo = NULL
  # verbose = 5
  # debug = TRUE
  # rap_reference = RAPS::load_demo_dataset("FAS")
  # cat(crayon::bold("CAUTION:", "USING DEMO MODE"))
  ####################################################
  ### TODO: Add this demo to the demo section
  # demo_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/stochastic_001_model_001."
  ####################################################


  ######################################
  # DEMO
  ######################################
  if (!is.null(demo)) {
    if (demo == 1) {
      ##################
      ##### Demo 1 #####
      ##################
      cat("Using the", crayon::bold("UPDATED"), "demo 1\n")
      n_rules = 6

      expected_exit = list(

        "Configuration" = tibble::tibble(
          environment = c("dummy_env", 0, 0, 0, 0),
          id = c("dummy_id", 0, 1, 2, 3),
          label = c("dummy_label", 0, 1, 2, 3), # Both children have the same label
          objects = list(
            tibble::tibble(object = "a", multiplicity = 1),
            tibble::tibble(),
            tibble::tibble(object = c("a", "b"),
                           multiplicity = 1:2),
            tibble::tibble(object = c("c", "d"),
                           multiplicity = 3:4),
            tibble::tibble(object = c("e", "f"),
                           multiplicity = 5:6)
          ),
          superM = c(NA, NA, 0, 0, 2), # Given by ID # END: We could have more than one parent

          subM = list(
            tibble::tibble(children = NA),
            tibble::tibble(children = c(1, 2)),
            tibble::tibble(children = NA),
            tibble::tibble(children = 3),
            tibble::tibble(children = NA)),
          charge = c(0, 0, 1, -1, 0),
          other_params = c(NA, NA, NA, NA, NA)
        ),

        "Rules" = tibble::tibble(
          # rule_id = 1:n_rules, # Basic
          rule_id = c("Dummy_rule", "Evolution_1", "Evolution_2", "In", "Out", "In-out"), # Advanced
          dissolves = rep(FALSE, n_rules), # TODO: Implement dissolution rules
          priority = c("-", as.character(1:(n_rules-1))),

          main_membrane_label = c("dummy_id", 1, rep(2, n_rules-2)),

          lhs = list(
            # Dummy: [a -> a]_dummy_id
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            # Evolution_1: [a,b2 -> a,b2,ap3,bp4]_1
            tibble::tibble(where = c("@here", "@here"),
                           object = c("a", "b"),
                           multiplicity = 1:2),
            # Evolution_2: [c3,d4 -> c3,d4]_2
            tibble::tibble(where = c("@here", "@here"),
                           object = c("c", "d"),
                           multiplicity = 3:4),
            # In: [d []_3 -> [fp]_3]_2
            tibble::tibble(where = c("@here", "@exists"),
                           object = c("e", "3"),
                           multiplicity = c(1, 1)),
            # Out: [[e]_3 -> cp []_3]_2
            tibble::tibble(where = c("3"),
                           object = c("e"),
                           multiplicity = 1),
            # In-out: [d[e]_3 -> cp [fp]_3]_2
            tibble::tibble(where = c("@here", "@exists", "3"),
                           object = c("d", "3", "e"),
                           multiplicity = c(1, 1, 1))
          ),

          rhs = list(
            # Dummy: [a -> a]_dummy_id
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            # Evolution_1: [a,b2 -> a,b2,ap3,bp4]_1
            tibble::tibble(where = rep("@here", 4),
                           object = c("a", "b", "ap", "bp"),
                           multiplicity = 1:4),
            # Evolution_2: [c3,d4 -> c3,d4]_2
            tibble::tibble(where = rep("@here", 2),
                           object = c("c", "d"),
                           multiplicity = 3:4),
            # In: [d []_3 -> [fp]_3]_2
            tibble::tibble(where = c("3"),
                           object = c("fp"),
                           multiplicity = 1),
            # Out: [[e]_3 -> cp []_3]_2
            tibble::tibble(where = c("@here"),
                           object = c("cp"),
                           multiplicity = 1),
            # In-out: [d[e]_3 -> cp [fp]_3]_2
            tibble::tibble(where = c("@here", "3"),
                           object = c("cp", "fp"),
                           multiplicity = c(1, 1))
          ),
          propensity = 1:n_rules
        ),

        "Properties" = tibble::tibble(
          System = 1,
          PLingua_model = "Transition",
          N_membranes = 4,
          N_objects = NA,
          N_rules = 11,
          Max_depth_in_rules = 1 # For now at least
        )
      )
    } else if (demo == 2) {
      ##################
      ##### Demo 2 #####
      ##################
      cat("Using the", crayon::bold("complex-enough"), "demo 2\n")

      expected_exit = list(

        "Configuration" = tibble::tibble(
          environment = c(0, 0, 0),
          id = c(0, 1, 2),
          label = c(0, 1, 2), # Both children have the same label
          objects = list(
            tibble::tibble(object = "@filler", multiplicity = 1),
            tibble::tibble(object = c("a", "b", "c", "d"),
                           multiplicity = 1:4),
            tibble::tibble(object = c("a", "b"), multiplicity = 1:2)
          ),
          superM = c(NA, 0, 1), # Given by ID # END: We could have more than one parent

          subM = list(
            tibble::tibble(children = 1),
            tibble::tibble(children = 2),
            tibble::tibble(children = NA)),
          charge = c(0, 1, -1),
          other_params = c(NA, NA, NA)
        ),

        "Rules" = tibble::tibble(
          # Rules:
          # 1. a --> b
          # 2. a --> b*2
          # 3. a --> b*2, c
          # 4. b*2 --> c
          # 5. a, b --> c
          # 6. a, b*2 --> c
          # 7. a, b*2 --> c*3
          # 8. a, b*2 --> c*3, d*4
          # 9. a --> lambda
          # 10. [a [ ]'2 --> [a]'2]'1
          # 11. [ [a]'2 --> a [ ]'2]'1
          # 12. a --> NEW
          # 13. [a [a]'2  -> [b]'2]'1
          # 14. [a,b []'2 -> [b]'2]'1
          # 15. [a []'2  -> b [c]'2]'1
          # 16. [b*2 []'2  -> [c*3]'2]'1
          # 17. [ [a,b]'2  -> b [c]'2]'1
          # 18. [ [a,b*2]'2  -> [c*3, d*4]'2]'1

          # Propensity = (11 - Rule number) / 11

          rule_id = 1:18,
          dissolves = c(rep(FALSE, 8), TRUE, rep(FALSE, 9)),
          priority = rep("-", 18),

          # lhs_membrane_label = rep(1, 12),
          main_membrane_label = rep(1, 18),
          lhs = list(
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "b",
                           multiplicity = 2),
            tibble::tibble(where = c("@here", "@here"),
                           object = c("a", "b"),
                           multiplicity = c(1,1)),
            tibble::tibble(where = c("@here", "@here"),
                           object = c("a", "b"),
                           multiplicity = 1:2),
            tibble::tibble(where = c("@here", "@here"),
                           object = c("a", "b"),
                           multiplicity = 1:2),
            tibble::tibble(where = c("@here", "@here"),
                           object = c("a", "b"),
                           multiplicity = 1:2),
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = c("@here", "@exists"),
                           object = c("a", 2),
                           multiplicity = c(1, 1)),
            tibble::tibble(where = 2,
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),

            ## TODO
            # 13. [a [a]'2  -> [b]'2]'1
            tibble::tibble(where = c("@here", 2),
                           object = c("a", "a"),
                           multiplicity = c(1, 1)),

            # 14. [a,b []'2 -> [b]'2]'1
            tibble::tibble(where = c("@here", "@here"),
                           object = c("a", "b"),
                           multiplicity = c(1, 1)),

            # 15. [a []'2  -> b [c]'2]'1
            tibble::tibble(where = c("@here", "@exists"),
                           object = c("a", 2),
                           multiplicity = c(1, 1)),

            # 16. [b*2 []'2  -> [c*3]'2]'1
            tibble::tibble(where = c("@here", "@exists"),
                           object = c("b", 2),
                           multiplicity = c(2, 1)),

            # 17. [ [a,b]'2  -> b [c]'2]'1
            tibble::tibble(where = c(2, 2),
                           object = c("a", "b"),
                           multiplicity = c(1, 1)),

            # 18. [ [a,b*2]'2  -> [c*3, d*4]'2]'1
            tibble::tibble(where = c(2, 2),
                           object = c("a", "b"),
                           multiplicity = c(1, 2))
          ),

          # rhs_membrane_label = rep(1, 12), # Replaced with main_membrane_label
          rhs = list(
            tibble::tibble(where = "@here",
                           object = "b",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "b",
                           multiplicity = 2),
            tibble::tibble(where = c("@here", "@here"),
                           object = c("b", "c"),
                           multiplicity = c(2,1)),
            tibble::tibble(where = "@here",
                           object = "c",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "c",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "c",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "c",
                           multiplicity = 3),
            tibble::tibble(where = c("@here", "@here"),
                           object = c("c", "d"),
                           multiplicity = 3:4),
            tibble::tibble(where = "@here",
                           object = "@delta",
                           multiplicity = 1),
            tibble::tibble(where = 2,
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "a",
                           multiplicity = 1),
            tibble::tibble(where = "@here",
                           object = "NEW",
                           multiplicity = 1),

            # 13. [a [a]'2  -> [b]'2]'1
            tibble::tibble(where = 2,
                           object = "b",
                           multiplicity = 1),

            # 14. [a,b []'2 -> [b]'2]'1
            tibble::tibble(where = 2,
                           object = "b",
                           multiplicity = 1),

            # 15. [a []'2  -> b [c]'2]'1
            tibble::tibble(where = c("@here", 2),
                           object = c("b", "c"),
                           multiplicity = c(1, 1)),

            # 16. [b*2 []'2  -> [c*3]'2]'1
            tibble::tibble(where = 2,
                           object = "c",
                           multiplicity = 3),

            # 17. [ [a,b]'2  -> b [c]'2]'1
            tibble::tibble(where = c("@here", 2),
                           object = c("b", "c"),
                           multiplicity = c(1, 1)),

            # 18. [ [a,b*2]'2  -> [c*3, d*4]'2]'1
            tibble::tibble(where = c(2, 2),
                           object = c("c", "d"),
                           multiplicity = 3:4)
          ),
          propensity = seq(1, 1/18, -1/18)
        ),

        "Properties" = tibble::tibble(
          System = NA,
          PLingua_model = NA,
          N_membranes = 2,
          N_objects = NA,
          N_rules = NA,
          Max_depth_in_rules = NA # For now at least
        )
      )
    } else if (demo == "small") {
      ########################
      ##### Demo "small" #####
      ########################
      cat("Using the", crayon::bold("OUTDATED"), "demo \"small\"\n")
      n_rules = 3

      expected_exit = list(
        Configuration = tibble::tibble(
          environment = c("env_1", "env_1", "env_1", "env_1"),
          id = c("skin", "id_1", "id_2", "id_3"),
          label = c("skin_label", "label_1", "label_2", "label_3"),
          objects = list(
            tibble::tibble(object = "@filler", multiplicity = 1),
            tibble::tibble(object = c("object_1", "object_999"), multiplicity = c(1, 999)),
            tibble::tibble(object = c("object_2"), multiplicity = c(2)),
            tibble::tibble(object = "object_n", multiplicity = 6023)
          ),
          superM = c(NA, "skin", "id_1", "id_2"), # Given by ID
          subM = list(
            tibble::tibble(children = "id_1"),
            tibble::tibble(children = "id_2"),
            tibble::tibble(children = "id_3"),
            tibble::tibble(children = NA)
          ),
          charge = c("-", "-", "-", "-"),
          other_params = c(NA, NA, NA, NA)
        ),
        Rules = tibble::tibble(
          rule_id = 1:n_rules,
          dissolves = c(rep(FALSE, n_rules)),
          priority = rep("-", n_rules),

          main_membrane_label = c(
            "skin", "skin", "id_1"
          ),
          lhs = list(
            tibble::tibble(where = c("@here"),
                           object = c("object_1"),
                           multiplicity = c(1)),
            tibble::tibble(where = c("@here", "@exists"),
                           object = c("object_999", "id_2"),
                           multiplicity = c(1, 1)),
            tibble::tibble(where = c("@here"),
                           object = c("object_2"),
                           multiplicity = c(1))
          ),
          rhs = list(
            tibble::tibble(where = c("@here"),
                           object = c("rule_1_executed"),
                           multiplicity = c(1)),
            tibble::tibble(where = c("@here"),
                           object = c("rule_2_executed"),
                           multiplicity = c(1)),
            tibble::tibble(where = c("@here"),
                           object = c("rule_3_executed"),
                           multiplicity = c(1))
          ),
          propensity = c(1:n_rules)
        ),
        Properties = NA
      )
    }

    return(expected_exit)
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
    return(length(var) == 0)
  }
  ## Examples
  # is_empty(NA)
  # is_empty(list())
  # is_empty(NULL)
  # is_empty(tibble())


  ### if(is.na or is.null) {exit} else {var}
  ## TODO: Substitute with tidyr::replace_na(var, new_element = "-"), which is compatible w/ mutate & friends
  substitute_if_empty = function(var, new_element = "-") {
    if (is.na(var) || is.null(var) || is_empty(var) || length(var) == 0 || var == "") {
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

  ## Property: Objects used in the PS
  objects_node = data_xml %>%
    xml2::xml_find_all("//objects")

  properties$objects = objects_node %>%
    xml2::xml_children() %>%
    xml2::xml_contents() %>%
    xml2::xml_text()

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
  properties$features = NA
  # features_node = data_xml %>%
  #   xml2::xml_find_all("//features")
  #
  # properties$features = features_node %>%
  #   magrittr::extract(1) %>%
  #   xml2::xml_children() %>%
  #   xml2::xml_text()

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

  ## TODO: Property: Semantics (rule information)
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
  get_objects_from_values = function(values, rhs = FALSE) {
    # Input: nodeset of valuei, each with a branch with id and, optionally, multiplicity

    # LHS - Debugging
    # values = lhs_info %>%
    #   xml2::xml_find_all(".//multiset")

    # RHS - Debugging
    values = rhs_info %>%
      xml2::xml_find_all(".//membranes") %>%
      xml2::xml_find_all(".//muiltiset")


    value_children = values %>%
      xml2::xml_children()


    if (rhs) {
      value_children %>% xml2::xml_children()
    }

    n_children = length(value_children) # > 1 by definition of the valuei fields

    object = c()
    multiplicity = c()

    for (child in 1:n_children) {
      aux_children = value_children[child] %>%
        xml2::xml_children()

      if (!rhs) {
        new_object = aux_children[1] %>%
          xml2::xml_text() # Can be more than one

        new_multiplicity = aux_children[2] %>%
          xml2::xml_text()

      } else {
        cat(crayon::bold("Not"), "working for RHS")
        new_object = NULL
        new_multiplicity = NULL

        # new_object = aux_children[2] %>%
        #   xml2::xml_children()
        #   xml2::xml_text()
        #
        # new_multiplicity = aux_children[2] %>%
        #   xml2::xml_text()

      }

      object %<>% c(new_object)
      multiplicity %<>% c(new_multiplicity)
    }

    return(tibble::tibble(object, multiplicity))
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

    multisets_aux$objects[[1]] = get_objects_from_values(value_node)

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
  verbose_print(cat(crayon::bold("labels"), "are not supported yet"), 2)
  configuration$labels = configuration$id

  ######################################
  # charge
  ######################################
  verbose_print(cat(crayon::bold("charge"), "is not supported yet"), 2)
  configuration$charge = "-"

  ######################################
  # other_params
  ######################################
  verbose_print(cat(crayon::bold("other_params"), "are not supported yet"), 2)
  configuration$other_params = NA

  ######################################
  exit$Configuration = configuration
  ######################################


  ##############################################################################
  ##############################################################################
  ################################ Rules #######################################
  ##############################################################################
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

  ##############################################################################
  get_rule_from_value = function(value) {
    children = value %>%
      xml2::xml_children()

    lhs_node = value %>% xml2::xml_find_all(".//left_hand_rule")
    rhs_node = value %>% xml2::xml_find_all(".//right_hand_rule")
    features_node = value %>% xml2::xml_find_all(".//features")

    ###############################################
    # lhs
    # In the demo the first rules are: [a -> b]'1, [a -> b*2]'1
    lhs_info = lhs_node %>%
      xml2::xml_children() %>% # multiset, membrane (note that there is no "s")
      magrittr::extract(2) # charge, label, multiset, children

    ## charge
    new_lhs_charge = lhs_info %>%
      xml2::xml_find_all(".//charge") %>%
      xml2::xml_text() %>%
      substitute_if_empty()

    if (new_lhs_charge != "-") {
      verbose_print(cat(crayon::bold("charge"), "in a rule is not supported yet"), 2)
    }

    ## main_membrane_label
    new_lhs_main_membrane_label = lhs_info %>%
      xml2::xml_find_all(".//label") %>%
      xml2::xml_text()

    ## multiset
    new_lhs_objects = lhs_info %>%
      xml2::xml_find_all(".//multiset") %>%
      get_objects_from_values()

    ## children
    new_lhs_children = lhs_info %>%
      xml2::xml_find_all(".//children")
    if (!is_empty(new_lhs_children)) {
      verbose_print(cat(crayon::bold("children"), "parameter is not supported yet"), 4)
    }

    ###############################################
    # rhs
    # In the demo the first rules are: [a -> b]'1, [a -> b*2]'1
    rhs_info = rhs_node # multiset, membranes (note the "s")

    ## main_membrane_label
    new_rhs_main_membrane_label = new_lhs_main_membrane_label

    ## multiset
    # rhs_info %>%
    #   xml2::xml_find_first(".//multiset")

    ## membranes
    membranes = rhs_info %>%
      xml2::xml_find_all(".//membranes")

    ## FIXME: Working w/ get_objects_from_values

    # n_membranes = length(membranes)
    # new_rhs_objects = tibble::tibble(object = NULL, multiplicity = NULL)
    # for (i in 1:n_membranes) {
    #   new_objects = get_objects_from_value(membranes[i])
    #   ## FIXME: Define a get_objects_from_value_rhs() function perhaps
    #   new_lhs_objects %<>%
    #     dplyr::bind_rows(new_objects)
    # }

    ## children
    new_lhs_children = lhs_info[3]
    if (!is_empty(new_lhs_children)) {
      verbose_print(cat(crayon::bold("children"), "parameter is not supported yet"), 4)
    }

    ###############################################
    # Parameters for RAPS
    main_membrane_label = sample(c(new_lhs_main_membrane_label,
                                   new_rhs_main_membrane_label), 1)


    ###############################################
    # features
    # In the demo the first rules with stochastic constants are: [a -> b]'1 @sc=1


    new_tibble = tibble::tibble(
      rule_id = NULL,
      dissolves = NULL,
      priority = NULL,
      main_membrane_label = NULL,
      lhs = NULL,
      rhs = NULL,
      propensity = NULL
    )

    return(new_tibble)
  }
  ##############################################################################

  n_rules = length(rules_value)

  ####################################
  exit$Properties$n_rules = n_rules
  ####################################

  for (i in 1:n_rules) {
    rules %<>%
      dplyr::bind_cols(get_rule_from_value(rules_value[i]))
  }


  exit$Rules = rules


  ##############################################################################
  ##############################################################################
  ################################## OLD #######################################
  ##############################################################################
  ##############################################################################

  rules_xml_nodeset = "TODO"
  n_rules = length(rules_xml_nodeset)
  exit$Properties %<>%
    dplyr::mutate("N_rules" = n_rules)

  cat("\n####################################################\n")
  for (i in 1:n_rules) { # For each rule
    new_rule = tibble::tibble(rule_id = i)
    cat(crayon::bold("r_", i, "\n", sep = ""))

    ####################
    # Dissolution

    ####################
    # Priority

    ####################
    # Stochastic constant

    ####################
    # LHS

    ####################
    # RHS

    ####################
    # Adding the rule to the previous ones
    exit$Rules %<>%
      dplyr::bind_rows(new_rule)
    cat("####################################################\n")
  }

  return(exit)
}
