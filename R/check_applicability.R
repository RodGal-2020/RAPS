#' Check if a rule can be applied
#'
#' `r lifecycle::badge("stable")`
#' Used inside application functions, this checks if a given rule can be applied. The parameter may be a little obscure, as it is used in a very specific moment of the workflow.
#'
#' @param affected_membranes A subset of `rap$Configuration` for a given `rap` object, with all the membranes affected by the application of the rule included in `rule_info`.
#' @param main_membrane_index The index of the main membrane within the `affected_membranes` object.
#' @param rule_info A row of `rap$Rules` for a given `rap` object, *i,e*, everything required to know a certain rule.
#' @param verbose The verbosity, between 0 and 5.
#'
#' @return
#' Nothing, only prints a message if everything is OK, and stops the execution otherwise.
#'
#' @export
check_applicability = function(rap, rule_info, verbose = 0) {

  # (rule_info = rap$Rules[21, ]) # Debugging

  rule_info$lhs[[1]] %<>%
    dplyr::rename(rule_multiplicity = multiplicity)
  rule_info$rhs[[1]] %<>%
    dplyr::rename(rule_multiplicity = multiplicity)

  # here = affected_membranes[main_membrane_index, ] # Quickfix A
  affected_membranes = rap$Configuration # Quickfix A
  here = rap$Configuration %>%
    dplyr::filter(id == rule_info$main_membrane_label)

  ##############################################################################
  verbose_print = function(action, minimum_verbose_to_print = 1) {
    if (verbose >= minimum_verbose_to_print) {
      action
    }
  }
  ##############################################################################

  ##############################################################################
  check_lhs_object = function(lhs_object) {
    ## Debugging
    # (lhs_object = rule_info$lhs[[1]][i, ])

    where = lhs_object$where

    # HERE
    ##########################
    if (where == "@here") {
      verbose_print(cat("\nChecking", crayon::bold("@here"), "element."), 2)

      mult = here$objects[[1]] %>%
        dplyr::filter(object == lhs_object$object) %$%
        multiplicity %>%
        sum(0)

      if (mult < lhs_object$rule_multiplicity) {
        RAPS::show_rule(rule_info)
        stop("ERROR: This rule cannot be applied.")
      }

    # @EXISTS
    ##########################
    } else if (where == "@exists") {
      verbose_print(cat("\nChecking", crayon::bold("@exists"), "element."), 2)

      if (! lhs_object$object %in% here$subM[[1]]) {
        RAPS::show_rule(rule_info)
        stop("ERROR: This rule cannot be applied due to the lack of a membrane.")
      }

    # H
    ##########################
    } else {
      verbose_print(cat("\nChecking", crayon::bold("standard"), "element."), 2)

      mult = affected_membranes %>%
        dplyr::filter(id == lhs_object$where) %$%
        objects %>%
        magrittr::extract2(1) %>%
        dplyr::filter(object == lhs_object$object) %$%
        multiplicity %>%
        sum(0)

      if (mult < lhs_object$rule_multiplicity) {
        RAPS::show_rule(rule_info)
        stop("ERROR: This rule cannot be applied.")
      }
    }
  }
  ##############################################################################

  lhs = rule_info$lhs[[1]]
  n_lhs = dim(lhs)[1]

  for (i in 1:n_lhs) {
    check_lhs_object(lhs[i, ])
    # We need to return the result from the previous one
  }

  verbose_print(cat("\nThe rule can be applied.\n"), 1)
}
