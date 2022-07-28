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

  if (dataset == "EGFR") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }

  if (dataset == "FAS") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }

  if (dataset == "QUORUM") {
    cat(crayon::bold("Function under development, returning NULL"))
    return(NULL)
  }

  if (dataset == TEMPLATE) {
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
                         object = "@lambda",
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
        N_rules = NA,
        Max_depth_in_rules = NA # For now at least
      )
    )
  }
}
