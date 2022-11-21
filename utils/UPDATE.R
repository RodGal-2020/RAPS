renv::status()
renv::snapshot()

cat("Change version manually in the DESCRIPTION file")
devtools::document() # To update the documentation
devtools::install() # To install as local package
# If "internal error -3" just restart R

devtools::check(error_on = "error")
devtools::check(error_on = "warning")
# Remember that the @examples section is checked.
