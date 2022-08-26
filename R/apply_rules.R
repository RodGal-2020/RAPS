#' Apply one or more rules to a given P system
#'
#' `r lifecycle::badge("experimental")`
#' Apply one or more ruless to a given P system given as a `rap` object.
#'
#' @param rap A rap object.
#' @param rules The set of rules to be applied.
#' @param return_between Do you want to return the middle states?
#'
#' @return
#' A new rap object, the result of applying the given rules.
#'
#' @seealso
#' `apply_rule_menv` to apply one rule in a multienvironmental P system, `apply_rule` to apply one rule in a monoenvironmental P system.
#'
#' @section TODO:
#' * Include examples
#'
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
