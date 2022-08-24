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

  ### UNCOMMENT TO TRACK ERRORS IN DEMO MODE
  ###############################
  # rap = RAPS::path2rap(demo = 1)
  # focus_on = list("MEM" = 2:3, "OBJ")
  # focus_on = list("RUL")
  ###############################

  names_focus = names(focus_on)

  if (is.null(focus_on)) {
    cat("\nReturning the default R visualization\n")
    rap
  } else {
    if ("MEM" %in% names_focus) {
      chosen_visualization = rap$Configuration %>%
        dplyr::filter(id %in% focus_on$MEM) # MEM is mandatory

      if ("OBJ" %in% focus_on) {
        chosen_visualization %<>%
          dplyr::select(id, objects)

        n_vis = dim(chosen_visualization)[1]

        for (i in 1:n_vis) {
          cat(rep("-", 50), sep = "")
          cat("\nShowing membrane", crayon::bold(chosen_visualization$id[i]), "\n")
          print(chosen_visualization$objects[[i]])
        }
      } else {
        # Returning all the chosen membranes
        chosen_visualization
      }
    } else if ("RUL" %in% names_focus) {
      print(rap$Rules)
    }
  }
}
