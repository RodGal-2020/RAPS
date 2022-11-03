#################################################
#################################################
################ R only file ####################
#################################################
#################################################
# install.packages("devtools") # For Rscript
devtools::install_github("Xopre/RAPS")
library(RAPS)
fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua5/RAPS/BIG/FAS.xml"

#############
coded = FALSE
#############

if (coded) {
  fas_rap = RAPS::path2rap(fas_path, use_codification = TRUE)
} else {
  fas_rap = RAPS::path2rap(fas_path, use_codification = FALSE)
}


################################################################################
# The function that will recover what we are interested in, in this case, the Bcl2,
# considering the original paper:
# get_Bcl2_from_rap = function(rap) {
#   if (coded) {
#     return(rap$Configuration %>%
#              dplyr::filter(id == coded_id) %$%
#              objects %>%
#              magrittr::extract2(1) %>%
#              dplyr::filter(object == "Bcl2") %$%
#              multiplicity)
#   } else {
#     return(rap$Configuration %>%
#              dplyr::filter(id == "m") %$% # This should work if we use the recodification
#              objects %>%
#              magrittr::extract2(1) %>%
#              dplyr::filter(object == "Bcl2") %$%
#              multiplicity)
#   }
# }
#
# get_Bcl2_from_rap(fas_rap)
################################################################################

################################################################################
# The function that will recover what we are interested in, in this case, the CASP3,
# considering the original paper. However, we will make it as general as possible:
get_concentration_of = function(rap, membrane_id, chosen_object) {
  labels = rap$Configuration$label
  ids = rap$Configuration$id
  coded_id = ids[which(ids == membrane_id)]

  if (coded) {
    return(rap$Configuration %>%
             dplyr::filter(id == coded_id) %$%
             objects %>%
             magrittr::extract2(1) %>%
             dplyr::filter(object == chosen_object) %$%
             multiplicity)
  } else {
    return(rap$Configuration %>%
             dplyr::filter(id == membrane_id) %$% # This should work if we use the recodification
             objects %>%
             magrittr::extract2(1) %>%
             dplyr::filter(object == chosen_object) %$%
             multiplicity)
  }
}

# get_concentration_of(fas_rap, membrane_id = "c", chosen_object = "CASP3")
################################################################################

## Utility
check_object = function(rap, focus) {
  # focus = "CASP3"
  for (i in 1:dim(rap$Rules)[1]) {
    rule = rap$Rules[i,]
    if (focus %in% rule$lhs[[1]]$object || focus %in% rule$rhs[[1]]$object) {
      cat(crayon::bold("\nRULE DETECTED\n"))
      RAPS::show_rule(rule)
    }
  }
}


set.seed(1974)
my_max_T = 1e-8

## Bcl2
# results = fas_rap %>%
#   RAPS::alg_det_menv(max_T = my_max_T, verbose = TRUE, debug = FALSE, save_each = get_Bcl2_from_rap)

## CASP3
# It appears only in membrane "c"
# check_object(fas_rap, "CASP3")
get_concentration_of_CASP3 = function(rap) {get_concentration_of(rap, membrane_id = "c", chosen_object = "CASP3")}
results = fas_rap %>%
  RAPS::alg_det_menv(max_T = my_max_T, verbose = TRUE, debug = FALSE, save_each = get_concentration_of_CASP3)

## CASP9_XIAP
# get_concentration_of_CASP9_XIAP = function(rap) {get_concentration_of(rap, membrane_id = "c", chosen_object = "CASP9_XIAP")}
# results = fas_rap %>%
#   RAPS::alg_det_menv(max_T = my_max_T, verbose = TRUE, debug = FALSE, save_each = get_concentration_of_CASP9_XIAP)

new_rap = results$final_rap
selected_data = results$selected_data
n_data = length(selected_data)

selected_data = tibble::tibble(i = 1:n_data, multiplicity = selected_data) %>%
  tidyr::unnest_longer(multiplicity)

(save_path = paste0("RData/max_T_", my_max_T, ".RData"))
save.image(save_path)


