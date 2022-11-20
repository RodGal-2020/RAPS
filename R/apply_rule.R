#' Apply a rule to a given P system
#'
#' `r lifecycle::badge("experimental")`
#' Apply a rule to a given P system given as a `rap` object.
#'
#' @param rap A rap object.
#' @param rule_id The id of the rule to be applied.
#' @param verbose The verbosity, between 0 and 5.
#' @param debug Useful if you want to debug the execution of the function.
#' @param keep_residue Do you want to delete the objects with multiplicity 0 at the end of the application?
#'
#' @return
#' A new rap object, the result of applying the given rule.
#'
#' @seealso
#' `apply_rules` to apply more than one rule at the same time, `apply_rule_menv` to apply one rule in a multienvironmental P system.
#'
#' @section TODO:
#' * Include examples
#'
#' @export
apply_rule = function(rap, rule_id, verbose = FALSE, debug = FALSE, keep_residue = FALSE) {

  ### Debugging
  ###############################
  # keep_residue = FALSE
  # verbose = 1
  # debug = TRUE
  ### FAS
  ## apply_rule_menv()
  # rap = affected_rap
  ## alg_det_menv
  # rule_id = i_0
  ###############################

  rule_info = RAPS::get_rule_by_id(rap, rule_id)
  # TODO: Compare w/ previous version:
  # rule_info = rap$Rules %>%
  #   dplyr::mutate(rule_id_col = rule_id) %>%
  #   dplyr::select(-rule_id) %>%
  #   dplyr::filter(rule_id_col == rule_id) %>%
  #   dplyr::mutate(rule_id = rule_id_col)

  # To directly avoid duplicates with dplyr
  rule_info$lhs[[1]] %<>%
    dplyr::rename(rule_multiplicity = multiplicity)
  rule_info$rhs[[1]] %<>%
    dplyr::rename(rule_multiplicity = multiplicity)

  ##############################
  # Check if it can be applied
  ##############################
  # RAPS::check_applicability(verbose = 1, affected_membranes, main_membrane_index, rule_info)
  RAPS::check_applicability(rap, rule_info, verbose = 1) # Quickfix


  if (verbose) {
    cat("\n\tApplying the rule with id", crayon::bold(rule_id))
    RAPS::show_rule(rule_info)
  }

  ####################################
  ###### Get affected membranes ######
  ####################################
  affected_membranes_labels = unique(c(rule_info$main_membrane_label)) # Quickfix

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

  affected_membranes = rap$Configuration %>%
    dplyr::filter(label %in% affected_membranes_labels)

  main_membrane_index = which(affected_membranes$label == rule_info$main_membrane_label)

  if (debug) {
    cat("\n\tDebug: affected_membranes$objects before anything = \n")
    print(affected_membranes$objects)
    RAPS::show_rule(rule_info)
  }

  #########################
  ###### Application ######
  #########################
  # Case w/ dissolution
  if (rule_info$dissolves) {
    cat("\nDissolution is yet to be implemented")
    # rap$Configuration %<>%
    #   dplyr::filter(label != rule_info$rhs_membrane_label) # We delete all the rhs membranes

    # Case w/o dissolution
  } else {

    ########################
    ###### Remove LHS ######
    ########################
    n_objects = dim(lhs)[1]

    for (i in 1:n_objects) {
      # i = 2 ## Debugging
      where = lhs[i, ]$where
      # object = lhs[i, ]$object
      # rule_multiplicity = lhs[i, ]$rule_multiplicity

      if (where == "@here") {
        affected_membranes[main_membrane_index, ]$objects[[1]] %<>%
          # affected_membranes[main_membrane_index, ]$objects[[1]] %>% # Debugging
          dplyr::left_join(lhs[i, ], by = "object") %>% # To preserve previous objects
          dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)

      } else if (where == "@exists"){
        # In this case we just check that it can be applied
        cat("\nChecking existance within labels, without using subM or superM")
        n_correct_membranes = sum(affected_membranes$label == lhs[i, ]$object)

        try(
          if (n_correct_membranes != lhs[i, ]$rule_multiplicity) {
            stop("\nERROR: Trying to apply a rule which can't be applied. Reference: @exists.")
          }
        )

      } else {
        # Membrane h
        secondary_membrane_index = which(affected_membranes$label == where)
        affected_membranes[secondary_membrane_index, ]$objects[[1]] %<>%
          # affected_membranes[secondary_membrane_index, ]$objects[[1]] %>% # Debugging
          dplyr::left_join(lhs[i, ], by = "object") %>% # To preserve previous objects
          dplyr::mutate(multiplicity = multiplicity - tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)
      }
    }


    if (debug) {
      cat("\n\tDebug: After LHS\n")
      cat("\n\tDebug: affected_membranes$objects = \n")
      print(affected_membranes$objects)
    }

    #######################
    ###### Add RHS ########
    #######################
    n_objects = dim(rhs)[1]

    for (i in 1:n_objects) {
      # i = 1 ## Debugging
      where = rhs[i, ]$where
      # affected_membrane_index = which(affected_membranes$label == where) # alternative

      if (where == "@here") {
        affected_membranes[main_membrane_index, ]$objects[[1]] %<>%
          dplyr::full_join(rhs[i, ], by = "object") %>%
          dplyr::mutate(multiplicity = tidyr::replace_na(multiplicity, 0) + tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)
        ## Alternative: add everything and then prune the special objects (like "@exists")

        # Some other membrane or "@exists"
      } else if (where == "@exists"){
        # This case could create a new membrane. For now, we will just return an error.
        stop("ERROR: @exists in RHS. Only correct with membrane creation")

      } else {
        secondary_membrane_index = which(affected_membranes$label == where)
        affected_membranes[secondary_membrane_index, ]$objects[[1]] %<>%
          # affected_membranes[secondary_membrane_index, ]$objects[[1]] %>% # Debugging
          dplyr::full_join(rhs[i, ], by = "object") %>% # To preserve previous objects
          dplyr::mutate(multiplicity = tidyr::replace_na(multiplicity, 0) + tidyr::replace_na(rule_multiplicity, 0), .keep = "all") %>%
          dplyr::select(object, multiplicity)
      }
    }

    if (debug) {
      cat("\nAfter RHS\n")
      cat("affected_membranes$objects = \n")
      print(affected_membranes$objects)
    }


    ##########################
    ###### Update rap ########
    ##########################
    rap$Configuration %<>%
    # rap$Configuration %>% ## Debugging
      # dplyr::filter(label != rule_info$rhs_membrane_label) %>% # Untouched ones
      dplyr::filter(!label %in% affected_membranes_labels) %>%
      # dplyr::bind_rows(affected_rhs_membranes) %>%
      dplyr::bind_rows(affected_membranes) %>%
      dplyr::arrange(label) # id could also work

  }

  ################################
  ####### Create fillers #########
  ################################
  l_objects = length(rap$Configuration$objects)
  for (i in 1:l_objects) {
    if (dim(rap$Configuration$objects[[i]])[1] == 0) {
      rap$Configuration$objects[[i]] = tibble::tibble(
        object = "@filler",
        multiplicity = "1"
      )
    }
  }


  ###########################
  ####### Clean rap #########
  ###########################

  if (!keep_residue) {
    for (i in 1:l_objects) {
      rap$Configuration$objects[[i]] %<>%
        dplyr::filter(multiplicity != 0)
    }
  }




  return(rap)
}
