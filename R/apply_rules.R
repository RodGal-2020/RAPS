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
apply_rules = function(rap, rules, return_between = FALSE) {
  cat("Needs: ", crayon::italic("apply_rule"), "?", sep = "")
  l = length(rules)

  if (return_between) {
    confs = c(rap)
    for (i in 1:l) {
      confs[i+1] = RAPS::apply_rule(confs[i], rules[i]) # Evolve to the following state
    }
    return(confs)
  } else {
    for (i in 1:l) {
      rap %<>% RAPS::apply_rule(rules[i])
      return(rap)
    }
  }
}
