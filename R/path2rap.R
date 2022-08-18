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


  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  # library(RAPS)
  # load("~/GitHub/RAPS/RData/path2rap.RData")
  #
  # online = FALSE
  # if (online) {
  #   path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/stochastic_model_001_RAPS_like_evolution.xml"
  # } else {
  #   # path = ".//example-psystems/plingua5/RAPS/plingua5/RAPS/stochastic_model_001_RAPS_like_evolution.xml"
  #   path = ".//.//stochastic_model_001_RAPS_like_evolution.xml"
  #   cat(crayon::bold("CAUTION:"), "Using a downloaded version of the xml.")
  # }
  # demo = NULL
  # verbose = TRUE
  # debug = TRUE
  # rap_reference = RAPS::load_demo_dataset("FAS")
  # cat(crayon::bold("CAUTION:", "USING DEMO MODE"))
  ##
  # TODO: Add this demo to the demo section
  ## demo_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/stochastic_001_model_001."
  ###


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
      cat("Using the demo 2\n")

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
      cat("Using the demo 'small'\n")
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
    if (is.na(var) || is.null(var) || is_empty(var)) {
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
  # Model info & properties
  ######################################
  properties = list()

  ## Property: PLingua output version
  properties$output_version = data_xml %>%
    xml2::xml_find_all('//version') %>%
    xml2::xml_text()

  # Keep only the psystem info
  data_xml %<>%
    magrittr::extract(3) %>% # psystem node
    xml2::xml_children()
  ## In the psystem node we have:
  # objects
  # labels
  # features
  # strings
  # max_multiplicity
  # model>id
  # semantics
  # structure
  # multisets
  # rules
  # features

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

  properties$labels = labels_node %>%
    xml2::xml_children() %>%
    xml2::xml_text() # May not be numbers

  ## Property: Features
  features_node = data_xml %>%
    xml2::xml_find_all("//features")

  properties$features = features_node %>%
    magrittr::extract(1) %>%
    xml2::xml_children() %>%
    xml2::xml_text()

  ## Property: Strings
  strings_node = data_xml %>%
    xml2::xml_find_all("//strings")

  properties$strings = strings_node %>%
    xml2::xml_children() %>%
    xml2::xml_text()

  ## Property: Max multiplicity
  max_multiplicity_node = data_xml %>%
    xml2::xml_find_all("//max_multiplicity")

  properties$max_multiplicity = max_multiplicity_node %>%
    xml2::xml_integer()

  ## Property: Model id
  model_id_node = data_xml %>%
    xml2::xml_find_all("//model")

  properties$model_id = model_id_node %>%
    xml2::xml_text()

  ## TODO: Property: Semantics (rule information)
  semantics = list()
  semantics_node_children = data_xml %>%
    xml2::xml_find_all("//semantics") %>%
    xml2::xml_children()
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
  verbose_print(cat("\n", crayon::bold("Semantics"), " (rule information) not supported yet", sep = ""))
  properties$semantics = semantics_node_children # Semantics not supported for now

  ######################################
  exit$Properties = properties
  ######################################

  ######################################
  # Configuration
  ######################################
  # Expected result (caso [[b*2, c*3]+'2 a]-'1)
  #   environment | id | label | objects         | superM   | subM | charge | other_params |
  #   ------------|----|-------|-----------------|----------|------|--------|--------------|
  #   1           | 1  | 1     | [(a, 1)]        | 0 (skin) | 2    |     -1 | NULL         |
  #   1           | 2  | 2     | [(b, 2), (c,3)] | 1        | NULL |     +1 | NULL         |

  ## xml outline
  # children >
  #             charge
  #             label > value0 > id
  #             children >
  #                         value0 >
  #                                   charge
  #                                   label > value0 > id
  #                                   children > ...
  #                         value1 >
  #                                   charge
  #                                   label > value0 > id
  #                                   children > ...

  #######################
  ### Membrane structure
  #######################


  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ### IDEA!!!!
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  my_tibble = tibble::tibble(xml = structure_node %>%
                               xml2::xml_children() %>%
                               xml2::as_list())

  my_tibble %>%
    # tidyr::unnest_wider(xml) %>%
    # tidyr::unnest_wider(xml) %>%
    tidyr::unnest_wider(xml) %>%
    tidyr::unnest_wider(1)

  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ### IDEA!!!! Look up there
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ##########################################

  ### subM & superM mainly
  structure_node = data_xml %>%
    xml2::xml_find_all("//structure")

  # initial_level = structure_node %>%
  initial_level = structure_node %>%
    xml2::xml_children() %>%
    magrittr::extract(2) %>%
    xml2::xml_find_all(".//id") %>%
    xml2::xml_text()

  cat("\nAssuming that only a root node exists") # Assuming or assumming or asumming?

  if (is_empty(initial_level)) {
    cat("\nInitial level not found, returning 0 status.")
    return(0)
  } else {
    initial_structure = list(initial_level) # Variable in which the initial structure will be saved. Used in order to avoid repetition while checking the initial configuration
    level = 1
  }

  children = structure_node %>%
    xml2::xml_children() %>%
    magrittr::extract(3)

  has_children = children %>%
    is_empty() %>%
    magrittr::not()

  while (has_children) {
    children %<>%
      xml2::xml_children()
  }

  children %>%
    xml2::xml_children()



  # focus_node = structure_node %>%
  new_son =  NA
    magrittr::extract(3) %>%
    # xml2::xml_find_all("//value*")
    is_empty() %>%
    magrittr::not()

  if (new_son) {
    initial_structure[[level]] %<>%
      append(branch)
  }




  structure_node %>% xml2::as_list() %>% View


  #######################
  ### do-while-like chunk
  #######################

  ## Compare with "Membrane structure" section
  level = 0

  structure_node_children = data_xml %>%
    xml2::xml_find_all("//structure") %>%
    xml2::xml_children()
  # charge, label, children

  ## charge
  charge = structure_node_children %>%
    # xml2::xml_find_all("//charge")
    magrittr::extract(1) %>%
    xml2::xml_text()

  ## label
  label = structure_node_children %>%
    # xml2::xml_find_all("//label")
    magrittr::extract(2) %>%
    xml2::xml_text()

  ## children
  children = structure_node_children %>%
    magrittr::extract(3) %>%
    xml2::xml_children() # One or more value_i > charge, label, children

  ## subM
  verbose_print(cat("\n", crayon::bold("subM"), " is under development", sep = ""))
  subM = children %>%
    xml2::xml_find_first("label")
  if (length(subM) == 0) {
    subM = NA
  } else {
    subM %<>%
      xml2::xml_children() %>%
      # xml2::xml_children() %>%
      xml2::xml_text()
  }


  configuration_tibble = tibble::tibble(
    environment = NA,
    id = NA,
    label,
    objects = NA,
    superM = list(NA),
    subM,
    charge,
    other_params = NA
  )

  n_children = length(children)

  level %<>% sum(1)

  while(n_children > 0) {
    cat("\tChecking level", level)

    for (branch in 1:n_children) {
      new_structure_node_children = children %>%
        magrittr::extract(branch) %>%
        xml2::xml_children()

      # charge, label, children
      ## charge
      charge = new_structure_node_children %>%
        # xml2::xml_find_all("//charge")
        magrittr::extract(1) %>%
        xml2::xml_text()

      ## label
      label = new_structure_node_children %>%
        # xml2::xml_find_all("//label")
        magrittr::extract(2) %>%
        xml2::xml_text()

      ## children
      children = new_structure_node_children %<>%
        magrittr::extract(3) %>%
        xml2::xml_children() # One or more value_i > charge, label, children

      ## subM
      verbose_print(cat("\n", crayon::bold("subM"), " is under development", sep = ""))
      subM = children %>%
        xml2::xml_find_first("label")
      if (length(subM) == 0) {
        subM = NA
      } else {
        subM %<>%
          xml2::xml_children() %>%
          # xml2::xml_children() %>%
          xml2::xml_text()
      }

      configuration_tibble %<>%
        dplyr::bind_rows(
          tibble::tibble(
            id = NA,
            label,
            objects = NA,
            superM = NA,
            subM,
            charge,
            other_params = NA
          )
        )

      n_children = length(children)
    }
  }

  ### Update superM
  n_membranes = dim(configuration_tibble)[1]
  verbose_print(cat("\n", crayon::bold("superM"), " is under development", sep = ""))
  for (row in 1:n_membranes) {
    subM = configuration_tibble[[row, "subM"]]

    if (!is.na(subM)) {
      for (subM_i in subM) {
        configuration_tibble %>%
          dplyr::filter(label == subM_i) %>%
          dplyr::mutate(superM =
                          ifelse(is.na(superM),
                                 subM_i,
                                 c(superM, subM_i)))
      }
    }
  }


  ######################################
  exit$Configuration = configuration_tibble
  ######################################


  ######################################
  # Rules
  ######################################
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
