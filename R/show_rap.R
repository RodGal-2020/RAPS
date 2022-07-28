#' Template for new functions
#'
#' This is a template.
#' @param my_param Yep, it's a parameter.
#' @return It returns...
#' @examples
#' Some examples
#' @section TODO:
#' * Improve visualization.
#' @export
show_rap = function(rap, focus_on = NULL) {
  if (is.null(focus_on)) {
    cat("Function under development, returning the default R visualization")
    rap
  }

}
