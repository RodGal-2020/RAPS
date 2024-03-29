#' Template for new functions
#'
#' @description
#' This is a template.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param rap A `rap` object.
#' @param verbose The verbosity, between 0 and 5.
#' @param debug Useful if you want to debug an execution of the function.
#'
#' @return
#' It returns...
#'
#' @examples
#' print("TODO:")
#'
#' @section Warning:
#' This is a warning
#'
#' @export
get_propensities = function(rap, verbose = 0, debug = FALSE) {

  ##############################################################################
  get_stochastic_constant = function(lhs_object, kinetic_constant, volume = 1) {
    # (rule = rules[92, ])
    # (lhs_object = rule$lhs)
    # (kinetic_constant = rule$kinetic_constant)

    lhs_object %<>%
    magrittr::extract2(1) %>%
    dplyr::filter(where != "@exists")
    n_objects_lhs = dim(lhs_object)[1]

    if (n_objects_lhs == 1) {
      if (lhs_object$multiplicity == 1) {
        return(kinetic_constant)
      } else if (lhs_object$multiplicity == 2) {
        # TODO: Test this, as it cannot be tested with FAS
        return(2 * kinetic_constant / volume)
      }

    } else if (all(lhs_object$multiplicity == 1)) {
      return(kinetic_constant / volume)

    } else {
      return(0)
      warning("Returning sc = 0. Strange rule detected")
    }
  }
  ##############################################################################

  ##############################################################################
  recode_where = function(rule, coded_object) {
    # (rule = rules[1, ]) # Debugging
    # (coded_object = rule$lhs[[1]]$object[1])
    where = rule$lhs[[1]] %>%
      dplyr::filter(where != "@exists") %>%
      dplyr::filter(object == coded_object) %$%
      where

    if (where == "@here") {
      return(rule$main_membrane_label)
    } else {
      return(where)
    }
  }
  ##############################################################################

  # Propensity is updated using the 7th page of https://www.cs.us.es/~marper/docencia/ARMB-2021-2022/temas/ARMB-tema-3-TRANS.pdf
  rules = rap$Rules

  ## Kinetic constants
  # Computed only once
  if (! "kinetic_constant" %in% colnames(rules)) {
    rules$kinetic_constant = rules$propensity
  }


  ## Stochastic constants
  # Computed only once
  if (! "stochastic_constant" %in% colnames(rules)) {
    rules %<>% dplyr::mutate(
        stochastic_constant = get_stochastic_constant(lhs, kinetic_constant, volume = 1),
        .keep = "all"
      )
  }

  ## Propensities
  # Main computing
  n_rules = dim(rules)[1]
  propensities = c()

  for (i in 1:n_rules) {
    # i = 1 # Debugging
    rule = rules[i, ]

    if (!RAPS::is_applicable(rap, rule)) {
      propensities %<>% c(0)
      next
    }

    lhs_object = rule$lhs[[1]] %>%
      dplyr::filter(where != "@exists") # Omitting presence of membrane/s
    n_objects_lhs = dim(lhs_object)[1]

    # rhs_object = rule$rhs[[1]]
    # n_objects_rhs = dim(rhs_object)[1]

    if (n_objects_lhs == 1 &&
        lhs_object$multiplicity == 1) {

      concentration = RAPS::get_concentration_of(
          rap,
          recode_where(rule, lhs_object$object),
          lhs_object$object
        )

      propensity = rule$stochastic_constant * concentration

    } else if (n_objects_lhs == 1 &&
               lhs_object$multiplicity == 2) {

      concentration = RAPS::get_concentration_of(
        rap,
        recode_where(rule, lhs_object$object),
        lhs_object$object
      )

      propensity = rule$stochastic_constant * 0.5 * concentration * (concentration - 1)


    } else if (n_objects_lhs > 1 &&
               all(lhs_object$multiplicity == 1)) {

      concentration_1 = RAPS::get_concentration_of(
        rap,
        recode_where(rule, lhs_object$object[1]),
        lhs_object$object[1]
      )

      concentration_2 = RAPS::get_concentration_of(
        rap,
        recode_where(rule, lhs_object$object[2]),
        lhs_object$object[2]
      )

      propensity = rule$stochastic_constant * concentration_1 * concentration_2

    } else {
      propensity = 0
      warning("Returning zero status. Strange rule detected")
    }

    propensities %<>% c(propensity)
  }

  rules$propensity = propensities

  return(rules)
}
