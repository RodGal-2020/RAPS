#################################################
#################################################
################ R only file ####################
#################################################
#################################################
devtools::install_github("Xopre/RAPS")
library(RAPS)
path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml"

fas_rap = RAPS::path2rap(path)


##########################################
##########################################
# ATTENTION
# As the labels are codified, this is used only within the get_Bcl2... function.
##########################################
##########################################
labels = fas_rap$Configuration$label
ids = fas_rap$Configuration$id
coded_id = ids[which(ids == "m")]

get_Bcl2_from_rap = function(rap) {
  return(rap$Configuration %>%
           # dplyr::filter(id == "m") %$% # This should work if we use the recodification
           dplyr::filter(id == coded_id) %$%
           objects %>%
           magrittr::extract2(1) %>%
           dplyr::filter(object == "Bcl2") %$%
           multiplicity)
}

get_Bcl2_from_rap(fas_rap)

set.seed(1974)
my_max_T = 1e-6
results = fas_rap %>%
  RAPS::alg_det_menv(max_T = my_max_T, verbose = TRUE, debug = FALSE, save_each = get_Bcl2_from_rap)

new_rap = results$final_rap
selected_data = results$selected_data
n_data = length(selected_data)

selected_data = tibble::tibble(i = 1:n_data, multiplicity = selected_data) %>%
  tidyr::unnest_longer(multiplicity)

(save_path = paste0("RData/max_T_", my_max_T, ".RData"))
save.image(save_path)


