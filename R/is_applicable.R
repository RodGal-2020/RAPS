#' Template for new functions
#'
#' @description
#' This is a template.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param my_param Yep, it's a parameter.
#'
#' @return
#' It returns...
#'
#' @examples
#' Some examples
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
