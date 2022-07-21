#' Omit all NAs in a given vector
#'
#' `r lifecycle::badge("deprecated")`
#' Used as help in some functions
#' @param v The vector.
#' @return The vector v without the NAs.
#' @examples
#' my_vector = c(1, 2, NA, 4, NA)
#' na_omit(my_vector)
#' @section Warning:
#' Experimental function.
#' @export
na_omit = function(v) {
  n = length(v)
  exit = c()

  for (i in 1:n) {
    if(!is.na(v[i])) {
      exit %<>% c(v[i])
    }
  }

  exit
}
