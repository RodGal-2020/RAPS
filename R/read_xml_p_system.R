#' Load a P System given as a XML file
#'
#' Bla bla bla
#' @param path Path to the input file
#' @param verbose Verbose?
#' @param print Show read data?
#' @return A loaded data.frame
#' @export
read_xml_p_system = function(path = NULL, verbose = TRUE, print = TRUE) {
  ### Aux functions
  # if(is.na) {exit} else {var}
  nago = function(var, ex = "-") {
    if (is.na(var)) {
      return(ex)
    } else {
      return(var)
    }
  }

  # Examples
  # nago(NA)
  # nago("+1")
  # nago(NA, ex = "0")


  # is.empty?
  is.empty = function(var) {
    return(length(var) == 0)
  }

  # if(empty) {act1} else {act2}
  ie.empty = function(var, act1, act2) {
    if (is.empty(var)) {
      act1
    } else {
      act2
    }
  }

  # Examples
  # is.empty(list())
  # is.empty(list("a"))
  # ie.empty(list(), cat("act1"), cat("act2"))
  # ie.empty(list("a"), cat("act1"), cat("act2"))


  # Missing parameters
  if(missing(path)) {
    cat()
    stop("Path required")
  }

  # Remember avoiding the "library()" function
  cat("Function under development")
}

## Error
# read_xml_p_system()
