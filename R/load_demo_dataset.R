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
load_demo_dataset = function(dataset = NULL) {
  datasets = c("EGFR", "FAS", "QUORUM", "TEMPLATE")
  if (is.null(dataset)) {
    stop("Expected a dataset from the following list: ", paste(datasets, collapse = ", "))
  }

  ##################################################
  ################# BEGINNING OF ###################
  ################# EGFR ###########################
  ##################################################
  if (dataset == "EGFR") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }
  ##################################################
  ################# END OF #########################
  ################# EGFR ###########################
  ##################################################




  ##################################################
  ################# BEGINNING OF ###################
  ################# FAS ############################
  ##################################################
  if (dataset == "FAS") {
    cat(crayon::bold("Returning fas_rap"))
    cat("\nBased on", crayon::italic("Simulating FAS-Induced Apoptosis by Using P Systems"), "by Smitha Cheruku, Andrei Păun, Francisco J. Romero-Campero, Mario J. Pérez-Jiménez, and Oscar H. Ibarra")
    fas_rap = list(

      "Configuration" = tibble::tibble(
        environment = c("e", "e", "e", "e"),
        id = c("e", "s", "c", "m"),
        label = c("e", "s", "c", "m"),
        objects = list(
          # e
          tibble::tibble(object = "FASL", multiplicity = 12500),
          # s
          tibble::tibble(object = "FASR",
                         multiplicity = 6023),
          # c
          tibble::tibble(object = c("FADD", "CASP8", "FLIP", "CASP3", "Bid", "Bax", "XIAP", "Apaf", "CASP9"),
                         multiplicity = c(10040, 20074, 48786, 120460, 15057, 50189, 18069, 60230, 12046)),
          # m
          tibble::tibble(object = c("Smac", "Cyto.c", "Bcl2"),
                         multiplicity = c(60230, 60230, 45172))
        ),
        superM = c(NA, "e", "s", "c"), # Given by ID # END: We could have more than one parent

        subM = list(
          tibble::tibble(children = "s"),
          tibble::tibble(children = "c"),
          tibble::tibble(children = "m"),
          tibble::tibble(children = NA)),
        charge = c("-", "-", "-", "-"),
        other_params = c(NA, NA, NA, NA)
      ),

      "Rules" = tibble::tibble(
        rule_id = 1:n_rules,
        dissolves = c(rep(FALSE, n_rules)),
        priority = rep("-", n_rules),

        main_membrane_label = rep(1, n_rules),

        lhs = list(
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("a", 2),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = "@here",
                         object = "a",
                         multiplicity = 1),
          tibble::tibble(where = c("@here", 2),
                         object = c("a", "a"),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = c("@here", "@here"),
                         object = c("a", "b"),
                         multiplicity = c(1, 1))
        ),

        rhs = list(
          tibble::tibble(where = c("@here", "@here"),
                         object = c("c", "d"),
                         multiplicity = 3:4),
          tibble::tibble(where = 2,
                         object = "b",
                         multiplicity = 1),
          tibble::tibble(where = c("@here", 2),
                         object = c("b", "c"),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = 2,
                         object = "c",
                         multiplicity = 3)
        ),
        propensity = seq(1, 1/n_rules, -1/n_rules)
      ),

      "Properties" = tibble::tibble(
        System = NA,
        PLingua_model = NA,
        N_membranes = NA,
        N_objects = 53,
        N_rules = 99,
        Max_depth_in_rules = NA # For now at least
      )
    )
    return(fas_rap)
  }
  ##################################################
  ################# END OF #########################
  ################# FAS ############################
  ##################################################




  ##################################################
  ################# BEGINNING OF ###################
  ################# QUORUM #########################
  ##################################################
  if (dataset == "QUORUM") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }
  ##################################################
  ################# END OF #########################
  ################# QUORUM #########################
  ##################################################





  ##################################################
  ################# BEGINNING OF ###################
  ################# TEMPLATE #######################
  ##################################################
  if (dataset == "TEMPLATE") {
    n_rules = 4
    template_exit = list(

      "Configuration" = tibble::tibble(
        environment = c(0, 0, 0),
        id = c(0, 1, 2),
        label = c(0, 1, 2),
        objects = list(
          tibble::tibble(object = "@filler", multiplicity = 1),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2),
          tibble::tibble(object = c("a", "b"),
                         multiplicity = 1:2)
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
        rule_id = 1:n_rules,
        dissolves = c(rep(FALSE, n_rules)),
        priority = rep("-", n_rules),

        main_membrane_label = rep(1, n_rules),

        lhs = list(
          tibble::tibble(where = c("@here", "@exists"),
                         object = c("a", 2),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = "@here",
                         object = "a",
                         multiplicity = 1),
          tibble::tibble(where = c("@here", 2),
                         object = c("a", "a"),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = c("@here", "@here"),
                         object = c("a", "b"),
                         multiplicity = c(1, 1))
        ),

        rhs = list(
          tibble::tibble(where = c("@here", "@here"),
                         object = c("c", "d"),
                         multiplicity = 3:4),
          tibble::tibble(where = 2,
                         object = "b",
                         multiplicity = 1),
          tibble::tibble(where = c("@here", 2),
                         object = c("b", "c"),
                         multiplicity = c(1, 1)),
          tibble::tibble(where = 2,
                         object = "c",
                         multiplicity = 3)
        ),
        propensity = seq(1, 1/n_rules, -1/n_rules)
      ),

      "Properties" = tibble::tibble(
        System = NA,
        PLingua_model = NA,
        N_membranes = NA,
        N_rules = NA,
        Max_depth_in_rules = NA # For now at least
      )
    )
    return(template_exit)
  }
  ##################################################
  ################# END OF #########################
  ################# TEMPLATE #######################
  ##################################################
}
