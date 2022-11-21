#' Print info of a given rule from a `rap object`
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Print a rule from a `rap` object.
#'
#' @param rule The rule from a `rap` object. **NOT** the id.
#'
#' @return
#' Nothing, only prints.
#'
#' @examples
#' rap = RAPS::path2rap(demo = 1)
#' show_rule(rap$Rules[1, ])
#'
#' @section TODO:
#' * Improve visualization.
#' * Add tibble-less mode in order to avoid unnecessary extra tibble info.
#'
#' @export
show_rule = function(rule) {
  cat("\n------------------------------\n")
  cat(crayon::bold("\n\tmain_membrane_label:"), rule$main_membrane_label)

  cat("\n------------------------------\n")
  crayon::bold("\n\tlhs:")
  print(rule$lhs[[1]])

  cat("\n------------------------------\n")
  crayon::bold("\n\tlhs:")
  print(rule$rhs[[1]])

  cat("\n------------------------------\n")
}
