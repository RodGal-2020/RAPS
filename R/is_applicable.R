#' Template for new functions
#'
#' @description
#' This is a template.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param rap A `rap` object.
#' @param rule_info A rule taken from a `rap$Rules` object. **NOT** an id.
#' @param verbose The verbosity, between 0 and 5.
#'
#' @return
#' It returns...
#'
#' @examples
#' print("TODO:")
#'
#' @section Warning:
#' This is a warning
#'
#' @export
is_applicable = function(rap, rule_info, verbose = 0) {
  tryCatch(
    expr = {
      RAPS::check_applicability(rap, rule_info, verbose)
      return(TRUE)
    },
    error = function(e){
      return(FALSE)
    },
    warning = function(w){},
    finally = {}
  )
}
