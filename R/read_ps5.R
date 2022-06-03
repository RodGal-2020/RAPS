#' Load a P System given as a XML/JSON file
#'
#' This functions admits a demo mode, loading predefined systems. It is the basic function of the package.
#' @param demo_mode Use demo settings? If FALSE path is mandatory.
#' @param path Path to the input file if necessary.
#' @param verbose Level of verbosity, between 0 and 5.
#' @return A list with a loaded tibble and the properties of the read system.
#' @examples
#' TODO
#' @section Warning:
#' Experimental function.
#' @export
read_ps5 = function(demo_mode = TRUE, path = NULL, verbose = 5) {

  cat("Using RAPS", packageDescription("RAPS", fields = "Version"), "\n\n")

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
              "Initial_config" = tibble::tibble(),
              "Properties" = tibble::tibble(System = 1, Note = "System ID included for generalisation"))


  ######################################
  # Basic data reading
  ######################################
  if (demo_mode) {
    cat("Using the demo mode with XML files\n") %>% verbose_print()
    cat("Choose between transition_i for i in 1:3u7:8\n")
    xml_file = readline()
    dir = paste0("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/", xml_file, ".xml")
    psystem_pli = paste0("https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/", xml_file, ".pli")

    psystem_pli %<>% readr::read_lines(n_max = 100)
    cat("Codification of the P system in .pli format:")
    cat("-----------------------------------------------------------\n")
    cat(psystem_pli, sep = "\n")
    cat("-----------------------------------------------------------\n")

    cat("Using the following demo directory:", dir, "\n") %>% verbose_print
  } else {
    if(missing(path)) {
      stop("Path required")
    }
    dir = path
    cat("Using the following custom directory:", case, "\n") %>% verbose_print
  }

  data_xml = xml2::read_xml(dir) %>%
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
  ## TODO: EIN?
  properties$max_multiplicity = data_xml %>%
    xml2::xml_find_all("//model") %>%
    xml2::xml_text()

  ## Property: Semantics
  ## HASTA AQUÍ
  data_xml %>%
    xml2::xml_find_all("//semantics") %>%
    xml2::xml_children()
    xml2::xml_text()
  semantics =


  model_type = data_xml %>%
    xml2::xml_attr("model")

  exit$Properties %<>%
    dplyr::mutate("PLingua_Model_type" = model_type)

  ######################################
  # Initial configuration
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

  ######################################
  # Expected exit
  ######################################
  expected_exit = list(
    "Rules" = tibble(
      rule_id = 1:2,
      dissolves = c(TRUE, FALSE),
      priority = c("-", "1"),
      lhs_multisets = c("For", "example"),
      lhs_membranes = c("For", "example"),
      rhs = c("The", "same")
      # read_xml_p_system:
      # rule_id dissolves priority lhs_outer_membrane_label lhs_outer_membrane_charge lhs_multisets rhs_outer_membrane_label rhs_outer_membrane_charge rhs_multisets lhs_inner_membranes rhs_inner_membranes rhs_inner_membranes_multisets lhs_inner_membranes_multisets
    ),
    "Initial_config" = tibble(
      label = 1:2,
      direct_descendants = list(as.character(3:4), NA),
      objects = list(
        tibble(object = c("a", "b"),
               multiplicity = 1:2),
        tibble(object = "c",
               multiplicity = 3)
      )
    ),
    "Properties" = tibble(
      System = 1,
      PLingua_model = "Transition",
      N_membranes = 4,
      N_rules = 11,
      Max_depth_in_rules = 1 # For now at least
    )
  )
}

