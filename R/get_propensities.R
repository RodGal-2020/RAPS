#' Template for new functions
#'
#' @description
#' This is a template.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param my_param Yep, it's a parameter.
#'
#' @return
#' It returns...
#'
#' @examples
#' Some examples
#'
#' @section Warning:
#' This is a warning
#'
#' @export
get_propensities = function(rap, verbose = 1, debug = FALSE) {

  ##############################################################################
  get_stochastic_constant = function(lhs_object, kinetic_constant) {
    lhs_object %<>%
      magrittr::extract2(1) %>%
      dplyr::filter(where != "@exists")
    n_objects_lhs = dim(lhs_object)[1]

    # rhs_object = chosen_rule$rhs[[1]]
    # n_objects_rhs = dim(rhs_object)[1]

    volume = 1

    if (n_objects_lhs == 1 &&
        lhs_object$multiplicity == 1) {
      return(kinetic_constant)

    } else if (n_objects_lhs == 1 &&
               lhs_object$multiplicity == 2) {
      return(2 * kinetic_constant / volume)

    } else if (n_objects_lhs > 1 &&
               all(lhs_object$multiplicity == 1)) {
      return(kinetic_constant / volume)

    } else {
      return(0)
      warning("Returning zero status. Strange rule detected")
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
        stochastic_constant = get_stochastic_constant(lhs, kinetic_constant),
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

    tryCatch(
      expr = {
        RAPS::check_applicability(rap, rule, verbose = 0)
      },
      error = function(e){ # If it cannot be applied
        propensity = 0
        propensities %<>% c(propensity)
      },
      warning = function(w){
        # (Optional)
        # Do this if an warning is caught...
      },
      finally = {
        # (Optional)
        # Do this at the end before quitting the tryCatch structure...
      }
    )

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
