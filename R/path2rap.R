#' Load a P System given as a path/URL to a XML/JSON file
#'
#' This is the basic function of the package, and allows us to read a given P system, turning it into a rap object.
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
path2rap = function(path = NULL, verbose = 5, demo = 1, max_depth = 3) {
  cat("Using RAPS", packageDescription("RAPS", fields = "Version"), "\n\n")

  ######################################
  # DEMO
  ######################################
  if (demo == 1) {
    ##################
    ##### Demo 1 #####
    ##################
    cat("Using the demo 1\n")

    expected_exit = list(

      "RAP" = tibble::tibble(
        environment = c(0, 0, 0, 0),
        id = c(0, 1, 2, 3),
        label = c(0, 1, 1, 2), # Both children have the same label
        objects = list(
          tibble::tibble(),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = c("c", "d"),
                         multiplicity = 3:4),
          tibble::tibble(object = c("e", "f"),
                         multiplicity = 5:6)
          ),
        superM = c(NA, 0, 0, 2), # Given by ID # END: We could have more than one parent

        subM = list(
          tibble::tibble(children = c(1, 2)),
          tibble::tibble(children = NA),
          tibble::tibble(children = 3),
          tibble::tibble(children = NA)),
        charge = c(0, 1, -1, 0),
        other_params = c(NA, NA, NA, NA)
      ),

      "Rules" = tibble::tibble(
        rule_id = 1:2,
        dissolves = c(FALSE, TRUE),
        priority = c("-", "1"),

        lhs_membrane_label = c(1,1),
        lhs = list(
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = c("c", "d"),
                         multiplicity = 3:4)
        ),

        rhs_membrane_label = c(1,1),
        rhs = list(
          tibble::tibble(object = c("a", "b", "ap", "bp"),
                         multiplicity = 1:4),
          tibble::tibble(object = c("c", "d"),
                         multiplicity = 3:4)
        ),
        propensity = c(0.4, 0.7)
      ),

      ## Included in rap
      # "Initial_config" = tibble::tibble(
      #   label = 1:2,
      #   direct_descendants = list(c("3", "4"), NA),
      #   objects = list(
      #     tibble::tibble(object = c("a", "b"),
      #            multiplicity = 1:2),
      #     tibble::tibble(object = "c",
      #            multiplicity = 3)
      #   )
      # ),

      "Properties" = tibble::tibble(
        System = 1,
        PLingua_model = "Transition",
        N_membranes = 4,
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

      "RAP" = tibble::tibble(
        environment = c(0, 0, 0),
        id = c(0, 1, 2),
        label = c(0, 1, 2), # Both children have the same label
        objects = list(
          tibble::tibble(),
          tibble::tibble(object = c("a", "b", "c", "d"),
                         multiplicity = 1:4),
          tibble::tibble()
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

        # Propensity = (11 - Rule number) / 11

        rule_id = 1:11,
        dissolves = c(rep(FALSE, 8), TRUE, FALSE, FALSE),
        priority = rep("-", 11),

        lhs_membrane_label = rep(1, 11),
        lhs = list(
          tibble::tibble(object = "a",
                         multiplicity = 1),
          tibble::tibble(object = "a",
                         multiplicity = 1),
          tibble::tibble(object = "a",
                         multiplicity = 1),
          tibble::tibble(object = "b",
                         multiplicity = 2),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = c(1,1)),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = "a",
                         multiplicity = 1),
          # TODO: 10, 11
          tibble::tibble(object = "TODO",
                         multiplicity = 1),
          tibble::tibble(object = "TODO",
                         multiplicity = 1)
        ),

        rhs_membrane_label = rep(1, 11),
        rhs = list(
          tibble::tibble(object = "b",
                         multiplicity = 1),
          tibble::tibble(object = "b",
                         multiplicity = 2),
          tibble::tibble(object = "b", "c",
                         multiplicity = c(2,1)),
          tibble::tibble(object = "c",
                         multiplicity = 1),
          tibble::tibble(object = "c",
                         multiplicity = 1),
          tibble::tibble(object = "c",
                         multiplicity = 1),
          tibble::tibble(object = "c",
                         multiplicity = 3),
          tibble::tibble(object = c("c", "d"),
                         multiplicity = 3:4),
          tibble::tibble(object = "@lambda",
                         multiplicity = 1),
          # TODO: 10, 11
          tibble::tibble(object = "TODO",
                         multiplicity = 1),
          tibble::tibble(object = "TODO",
                         multiplicity = 1)
        ),
        propensity = seq(1, 1/11, -1/11)
      ),

      "Properties" = tibble::tibble(
        System = NA,
        PLingua_model = NA,
        N_membranes = 2,
        N_rules = NA,
        Max_depth_in_rules = NA # For now at least
      )
    )
  }

  if (demo) {
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
  # verbose_print(cat("Hola!"), 2)


  ### TODO: Substitute with tidyr::replace_na(var, new_element = "-"), which is compatible w/ mutate & friends

  ### if(is.na or is.null) {exit} else {var}
  substitute_if_empty = function(var, new_element = "-") {
    if (is.na(var) || is.null(var)) {
      return(new_element)
    } else {
      return(var)
    }
  }
  ## Examples
  # substitute_if_empty(NA)
  # substitute_if_empty("+1")
  # substitute_if_empty(NA, new_element = "0")

  ### is.empty?
  is_empty = function(var) {
    return(length(var) == 0)
  }
  ## Examples
  # is_empty(NA)
  # is_empty(list())
  # is_empty(NULL)
  # is_empty(tibble())



  ######################################
  # Exit parameters
  ######################################
  exit = list("Rules" = tibble::tibble(),
              "RAP" = tibble::tibble(),
              # "Initial_config" = tibble::tibble(), # Included in RAP
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
    xml2::xml_children()

  ######################################
  # Model info & properties
  ######################################
  properties = list()

  ## Property: PLingua version
  properties$version = data_xml %>%
    xml2::xml_find_all('//version') %>%
    xml2::xml_contents() %>%
    xml2::xml_text()

  # Keep only the psystem info
  data_xml %<>%
    magrittr::extract(3) %>%
    xml2::xml_children()

  ## Property: Objects used in the PS
  properties$objects = data_xml %>%
    xml2::xml_find_all("//objects") %>%
    xml2::xml_children() %>%
    xml2::xml_contents() %>%
    xml2::xml_text()

  ## Property: Labels used in the membrane structure
  properties$labels = data_xml %>%
    xml2::xml_find_all("//labels") %>%
    xml2::xml_children() %>%
    xml2::xml_text() # May not be numbers

  ## Property: Features
  properties$features = data_xml %>%
    xml2::xml_find_all("//features") %>%
    magrittr::extract(1) %>%
    xml2::xml_children() %>%
    xml2::xml_text()

  ## Property: Strings
  properties$strings = data_xml %>%
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

  ## Property: Semantics
  semantics = list()
  semantics_aux = data_xml %>%
    xml2::xml_find_all("//semantics") %>%
    xml2::xml_children()
  semantics$value = semantics_aux %>% magrittr::extract(1) %>% xml2::xml_integer()
  semantics$inf = semantics_aux %>% magrittr::extract(2) %>% xml2::xml_text()
  if (semantics$inf == "true") {
    semantics$inf = TRUE
  } else {
    semantics$inf = FALSE
  }
  semantics$patterns = "TODO"
  semantics$children_size = semantics_aux %>% magrittr::extract(4) %>% xml2::xml_attr("size")

  properties$semantics = semantics

  ## TODO: Delete if useless
  exit$Properties %<>%
    # dplyr::mutate("PLingua_Model_type" = model_type)

  ######################################
  exit$Properties = properties
  ######################################

  ######################################
  # RAP
  ######################################
  # Expected result (caso [[b*2, c*3]+'2 a]-'1)
  #   Environment | ID | Label | Objects         | SuperM   | SubM | Charge | Other_params |
  #   ------------|----|-------|-----------------|----------|------|--------|--------------|
  #   1           | 1  | 1     | [(a, 1)]        | 0 (skin) | 2    |     -1 | NULL         |
  #   1           | 2  | 2     | [(b, 2), (c,3)] | 1        | NULL |     +1 | NULL         |

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

  rap = tibble(
    environment = 0,
    id = 0,
    label = 0,
    objects = NA,
    superM = NA,
    subM = NA,
    charge = 0, # Could be other
    other_params = NA
  )

  structure = data_xml %>%
    xml2::xml_find_all("//structure") %>%
    xml2::xml_children()
  # charge, label, size

  children = structure %>%
    magrittr::extract(3) %>%
    xml2::xml_children() # One or more value_i > charge, label, children

  n_children = length(children)

  ######################################
  # Structure

  if (n_children != 0) {
    for (level in 1:max_depth) {
      cat("\tChecking level", level)
      ### TODO: Complete taking into account the previous part

      for (branch in 1:n_children) {
        new_row_xml_i = children %>%
          magrittr::extract(branch) %>%
          xml2::xml_children()

        new_environment = 0

        new_id = new_row_xml_i %>%
          magrittr::extract(2) %>%
          xml2::xml_find_first(".//id") %>%
          xml2::xml_integer()

        new_label = new_row_xml_i %>%
          magrittr::extract(2) %>%
          xml2::xml_find_first(".//id") %>%
          xml2::xml_integer()

        new_objects = NA

        new_superM = NA

        new_subM = NA

        new_charge = 0

        new_other_params = NA

        rap %<>%
          dplyr::bind_rows(
            tibble::tibble(
              environment = new_environment,
              id = new_id,
              label = new_label,
              objects = new_objects,
              superM = new_superM,
              subM = new_subM,
              charge = new_charge,
              other_params = new_other_params
            )
          )

      }
    }
  } else {
    cat("Unexpected structure size. Empty P system?")
  }

  ######################################
  # Initial configuration

  ######################################
  exit$RAP = rap
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
