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
show_rule = function(rule) {
  cat("\n------------------------------\n")
  crayon::bold("\n\tlhs:")
  print(rule$lhs[[1]])

  cat("\n------------------------------\n")
  crayon::bold("\n\tlhs:")
  print(rule$rhs[[1]])

  cat("\n------------------------------")
}
