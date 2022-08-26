#' Print info of a given `rap` object
#'
#' @description
#' `r lifecycle::badge("stable")`
#' Print objects, rules or subsets of those of a given `rap` object.
#'
#' @param rap The `rap` object.
#' @param focus_on A sublistof `list("MEM" = 2:3, "OBJ")` or `list("RUL")`.
#'
#' @return
#' Nothing, only prints.
#'
#' @examples
#' rap = RAPS::path2rap(demo = 1)
#' focus_on = list("MEM" = 2:3, "OBJ")
#' show_rap(rap, focus_on)
#' focus_on = list("RUL")
#' show_rap(rap, focus_on)
#'
#' @section TODO:
#' * Improve visualization.
#' * Add syntax guide for the `focus_on` parameter.
#'
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
