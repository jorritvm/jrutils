#' returns the size of the global workspace in a human readable format
#'
#' @return the memory usage of the global environment
#' @export
#' @import utils
print_globalenv_size = function() {
  size = 0
  for (v123 in ls(envir = globalenv())) {
    size = size + object.size(get(v123))
  }
  print(format(size, units = "auto"))
}


#' returns the size of a single variable in a human readable format
#'
#' @param variableName name of the variable for which to display the memory usage.
#'
#' @return the size of the variable
#' @export
#' @import utils
print_var_size = function(variableName) {
  print(format(object.size(variableName), units = "auto"))
}


#' clear the R console in RSTUDIO
#'
#' @export
clc = function() {
  cat("\014") # sends CTRL+L keybind to console
}


#' \code{clear} will clear the global environment or, when specified, a list of
#' variables. \code{clear} will then call the garbage collector.
#'
#' @param x a vector of variable names to be deleted from the workspace
#'
#' @export
clear = function(x = c()) {
  if (length(x) == 0) x = ls()
}


#' closes any open plots in the RStudio viewer pane
#'
#' @export
clearplots = function() {
  if(!is.null(dev.list())) dev.off()
}


#' Provides information about the type of sessions we are running
#'
#' @return a boolean 'interactive' = TRUE when code is run from RStudio, FALSE if it is run from RScript.exe
#' @export
get_session_type = function() {
  if (commandArgs()[1] == "Rstudio") {
    # we are now in interactive mode running the script from rstudio  
    interactive = TRUE
  } else {
    # we are running the script using the R binary from some location (RScript.exe)  
    interactive = FALSE
  }
}
