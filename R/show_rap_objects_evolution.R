#' Print info of a given `rap` object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Print objects of a given `rap` object.
#'
#' @param rap The `rap` object.
#'
#' @return
#' Nothing, only prints.
#'
#' @examples
#' rap = RAPS::path2rap(demo = 1)
#' focus_on = list("MEM" = 2:3, "OBJ")
#' show_rap(rap, focus_on)
#' focus_on = list("RUL")
#'
#' @export
show_rap_objects_evolution = function(rap_objects) {

  print("Idea of the function in the comments & in deterministic_waiting_time_algorithm.R")

  ## Multiple variables

  # get_m_objects = function(rap) {
  #   return(rap$Configuration %>%
  #            dplyr::filter(id == "m") %$%
  #            objects %>%
  #            magrittr::extract2(1))
  # }
  #
  # get_m_objects(fas_rap)

  # set.seed(1974)
  # results = fas_rap %>%
  #   RAPS::alg_det_menv(max_T = 1e-5, verbose = TRUE, debug = FALSE, save_each = get_m_objects)
  #
  # new_rap = results$final_rap
  # selected_data = results$selected_data
  # n_data = length(selected_data)
  #
  # selected_data = tibble::tibble(i = 1:n_data, selected_data) %>%
  #   tidyr::unnest_longer(selected_data) %>%
  #   tidyr::unnest_wider(selected_data)
  #
  # diff_objects = unique(selected_data$object)
  #
  # for (iter in 1:n_data) {
  #   for (name in diff_objects) {
  #     if (dim(dplyr::filter(selected_data, object == name & i == iter))[1] == 0) {
  #       selected_data %<>%
  #         dplyr::bind_rows(
  #           tibble::tibble(
  #             i = iter,
  #             object = name,
  #             multiplicity = 0
  #           )
  #         )
  #     }
  #   }
  # }
  #
  # library(ggplot2)
  #
  # selected_data %>%
  #   ggplot(aes(x=i, y=multiplicity, group=object)) +
  #   geom_line(arrow = arrow())+
  #   geom_point()

}
