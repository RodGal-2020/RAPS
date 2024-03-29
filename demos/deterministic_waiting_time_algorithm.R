#################################################
#################################################
################ R only file ####################
#################################################
#################################################
# install.packages("devtools", repos='http://cran.us.r-project.org') # For Rscript
devtools::install_github("Xopre/RAPS")
library(RAPS)
fas_path = "https://raw.githubusercontent.com/Xopre/psystems-examples/main/plingua-5.0/RAPS/BIG/FAS.xml"

#############
coded = FALSE
#############

if (coded) {
  fas_rap = RAPS::path2rap(fas_path, use_codification = TRUE)
} else {
  fas_rap = RAPS::path2rap(fas_path, use_codification = FALSE)
}

## Utility
# check_object = function(rap, focus) {
#   # focus = "CASP3"
#   for (i in 1:dim(rap$Rules)[1]) {
#     rule = rap$Rules[i,]
#     if (focus %in% rule$lhs[[1]]$object || focus %in% rule$rhs[[1]]$object) {
#       cat(crayon::bold("\nRULE DETECTED\n"))
#       RAPS::show_rule(rule)
#     }
#   }
# }
# CASP3 only appears in membrane "c"
# check_object(fas_rap, "CASP3")


# my_max_T = 9*60 # Real example
my_max_T = 1 # Minimal example
# my_max_T = 1e-4
set.seed(1974)

setwd("RAPS") # Just in case

## CASP3
(save_path = paste0("RData/max_T_", my_max_T, ".RData"))
get_concentration_of_CASP3 = function(rap) {RAPS::get_concentration_of(rap, membrane_id = "c", chosen_object = "CASP3", is_coded = coded)}
get_concentration_of_CASP3(fas_rap) # Demo

## Main chunk
start = Sys.time()
results = fas_rap %>%
  RAPS::alg_det_menv(
    max_T = my_max_T,
    verbose = TRUE,
    debug = FALSE,
    save_each = get_concentration_of_CASP3); save.image(save_path); cat("\nProceso iniciado a las: \n"); print(start); cat("\ny terminado a las: \n"); Sys.time()
################################################################
# LAST LAUNCHED RULE
################################################################

new_rap = results$final_rap
selected_data = results$selected_data
n_data = length(selected_data)

selected_data = tibble::tibble(i = 1:n_data, multiplicity = selected_data) %>%
  tidyr::unnest_longer(multiplicity)

save.image(save_path)
# Alternative:
# save(new_rap, selected_data, n_data, file = save_path)


