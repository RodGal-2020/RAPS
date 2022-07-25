#' Apply a rule to a given rap object
#'
#' `r lifecycle::badge("experimental")`
#' @param rap A rap object-
#' @param rule_id The id of the rule to be applied-
#' @return A new rap object, the result of applying the given rule-
#' @examples
#' TODO
#' @seealso
#' `apply_rules` to apply more than one rule at the same time.
#' #' @section TODO:
#' * Print the trace of the execution, perhaps with the `RAPS::show_rap()` function.
#' @export
apply_rule = function(rap, rule_id, verbose = FALSE) {

  ### DELETE THIS DEMO
  # cat("\nUsing the demo rap...")
  # rap = RAPS::path2rap(demo = 2)
  # rule_id = 1
  ###

  rule_info = rap$Rules[rule_id, ]
  # To directly avoid duplicates with dplyr
  colnames(rule_info$lhs[[1]]) = c("where", "object", "rule_multiplicity")
  colnames(rule_info$rhs[[1]]) = c("where", "object", "rule_multiplicity")

  if (verbose) {
    cat("\n\tApplying the rule with id", crayon::bold(rule_id))
    RAPS::show_rule(rule_info)
  }

  ##############################
  # Check if it can be applied
  ##############################
  # TODO
  cat("\nWe can't check if the rule could be applied. Let's hope it could :)")


  ####################################
  ###### Get affected membranes ######
  ####################################
  affected_membranes_labels = c(rule_info$main_membrane_label)

  lhs = rule_info$lhs[[1]]
  rhs = rule_info$rhs[[1]]

  lhs_and_rhs_where = c(lhs$where, rhs$where)
  n_aux = length(lhs_and_rhs_where)

  for (i in 1:n_aux) {
    m_i = lhs_and_rhs_where[i]
    if (m_i != "@here" & !m_i %in% affected_membranes_labels) {
      affected_membranes_labels %<>% c(m_i)
    }
  }

  affected_membranes = rap$RAP %>%
    dplyr::filter(label %in% affected_membranes_labels)

  main_membrane_index = which(affected_membranes$label == rule_info$main_membrane_label)


  #########################
  ###### Application ######
  #########################
  # Case w/ dissolution
  if (rule_info$dissolves) {
    cat("\nDissolution is yet to be implemented")
    # rap$RAP %<>%
    #   dplyr::filter(label != rule_info$rhs_membrane_label) # We delete all the rhs membranes

  # Case w/o dissolution
  } else {

    ########################
    ###### Remove LHS ######
    ########################
    n_objects = dim(lhs)[1]

    for (i in 1:n_objects) {
      where = lhs[i, ]$where
      # object = lhs[i, ]$object
      # rule_multiplicity = lhs[i, ]$rule_multiplicity

      if (where == "@here") {
        affected_membranes[main_membrane_index, ]$objects[[1]] %<>%
          dplyr::left_join(rule_info$lhs[[1]], by = "object") %>% # To preserve previous objects
          dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)

      # Some other membrane or "@exists"
      } else if (where == "@exists"){
        # In this case we just check that it can be applied
        n_correct_membranes = sum(affected_membranes$label == lhs[i, ]$object)

        try(
          if (n_correct_membranes != lhs[i, ]$rule_multiplicity) {
            stop("ERROR: Trying to apply a rule which can't be applied. Reference: @exists.")
          }
        )

      } else {
        secondary_membrane_index = which(affected_membranes$label == where)
        affected_membranes[secondary_membrane_index, ]$objects[[1]] %<>%
          dplyr::left_join(rule_info$lhs[[1]], by = "object") %>% # To preserve previous objects
          dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)
      }
    }

    # considered_objects = affected_lhs_membranes %$%
    #   objects # a*1, b*2; c*3, d*4
    #
    # for (i in 1:length(considered_objects)) {
    #   affected_lhs_membranes$objects[[i]] %<>%
    #     dplyr::left_join(rule_info$lhs[[1]], by = "object") %>% # To preserve previous objects
    #     dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
    #     dplyr::select(object, multiplicity)
    # }

    ##########################
    ###### Update rap ########
    # rap$RAP %<>%
    #   dplyr::filter(label != rule_info$lhs_membrane_label) %>% # Untouched by the LHS
    #   dplyr::bind_rows(affected_lhs_membranes) %>%
    #   dplyr::arrange(label) # id could also work


    #######################
    ###### Add RHS ########
    #######################
    n_objects = dim(rhs)[1]

    for (i in 1:n_objects) {
      where = rhs[i, ]$where
      # affected_membrane_index = which(affected_membranes$label == where) # alternative

      if (where == "@here") {
        affected_membranes[main_membrane_index, ]$objects[[1]] %<>%
          dplyr::full_join(rule_info$rhs[[1]], by = "object") %>%
          dplyr::mutate(multiplicity = tidyr::replace_na(multiplicity, 0) + tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)

        # Some other membrane or "@exists"
      } else if (where == "@exists"){
        # This case could create a new membrane. For now, we will just return an error.
        stop("ERROR: @exists in RHS. Only correct with membrane creation")

      } else {
        secondary_membrane_index = which(affected_membranes$label == where)
        affected_membranes[secondary_membrane_index, ]$objects[[1]] %<>%
          dplyr::left_join(rule_info$rhs[[1]], by = "object") %>% # To preserve previous objects
          dplyr::mutate(multiplicity = multiplicity + tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)
      }
    }




    #######################
    ###### Add RHS OLD ########
    #######################
    # affected_rhs_membranes = rap$RAP %>%
    #   dplyr::filter(label == rule_info$rhs_membrane_label)
    #
    # considered_objects = affected_rhs_membranes %$%
    #   objects # c*3, d*4; e*5, f*6 modified perhaps by previous rule
    #
    # for (i in 1:length(considered_objects)) {
    #   affected_rhs_membranes$objects[[i]] %<>%
    #     dplyr::full_join(rule_info$rhs[[1]], by = "object") %>%
    #     dplyr::mutate(multiplicity = tidyr::replace_na(multiplicity, 0) + tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
    #     dplyr::select(object, multiplicity)
    # }

    ##########################
    ###### Update rap ########
    ##########################
    rap$RAP %<>%
      # dplyr::filter(label != rule_info$rhs_membrane_label) %>% # Untouched ones
      dplyr::filter(!label %in% affected_membranes_labels) %>%
      # dplyr::bind_rows(affected_rhs_membranes) %>%
      dplyr::bind_rows(affected_membranes) %>%
      dplyr::arrange(label) # id could also work

  }

  return(rap)
}
