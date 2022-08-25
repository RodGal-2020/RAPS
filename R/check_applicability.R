#' Template for new functions
#'
#' This is a template.
#' @param my_param Yep, it's a parameter.
#' @return It returns...
#' @examples
#' Some examples
#' @section Warning:
#' This is a warning
#' @export
check_applicability = function(verbose, affected_membranes, main_membrane_index, rule_info) {

  here = affected_membranes[main_membrane_index, ]

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
    # lhs_object = rule_info$lhs[[1]][1, ]
    # lhs_object = rule_info$lhs[[1]][2, ]

    where = lhs_object$where

    # HERE
    ##########################
    if (where == "@here") {
      verbose_print(cat("\nChecking", crayon::bold("@here"), "element."), 2)

      mult = here$objects[[1]] %>%
        dplyr::filter(object == lhs_object$object) %$%
        multiplicity

      if (mult < lhs_object$multiplicity) {
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

    # N
    ##########################
    } else {
      verbose_print(cat("\nChecking", crayon::bold("standard"), "element."), 2)

      mult = affected_membranes %>%
        dplyr::filter(id == lhs_object$where) %$%
        objects %>%
        magrittr::extract2(1) %>%
        dplyr::filter(object == lhs_object$object) %$%
        multiplicity

      if (mult < lhs_object$multiplicity) {
        RAPS::show_rule(rule_info)
        stop("ERROR: This rule cannot be applied.")
      }
    }
  }
  ##############################################################################

  lhs = rule_info$lhs[[1]]
  n_lhs = dim(lhs)[1]

  for (i in 1:n_lhs) {
    check_lhs_object(lhs[i])
  }

  verbose_print(cat("\nThe rule could be applied.\n"), 1)
}
