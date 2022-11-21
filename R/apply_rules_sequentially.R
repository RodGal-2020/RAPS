#' Apply one or more rules to a given P system
#'
#' `r lifecycle::badge("experimental")`
#' Apply one or more ruless to a given P system given as a `rap` object.
#'
#' @param rap A rap object.
#' @param rules_ids The ids of the rules to be applied.
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
apply_rules_sequentially = function(rap, rules_ids, return_between = FALSE) {
  n_rules = length(rules_ids)

  if (return_between) {
    confs = list(rap)
  }

  for (i in 1:n_rules) {
      rap %<>% RAPS::apply_rule(rules_ids[i]) # Evolve to the following state
      if (return_between) {
        confs %<>% append(list(rap))
      }
  }

  if (return_between) {
    return(confs)
  } else {
    return(rap)
  }
}
