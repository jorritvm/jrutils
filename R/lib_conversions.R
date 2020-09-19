#' create a list where for each element the name of the variable is the key and the content is the variable content
#'
#' @param ... comma seperated list of variables
#'
#' @return named list
#' @export
#'
#' @examples
#' a <- b <- c <- 1
#' namedList(a,b,c)
named_list <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}


#' converts a string to a boolean if it holds true or false, but retains the string if not
#'
#' @param x
#'
#' @return
#' @export
string_gently_to_boolean = function(x) {
  if  (!is.character(x)) return(x)
  
  if (tolower(x) == "true") {
    return(TRUE)
  } else if (tolower(x) == "false")  {
    return(FALSE)
  } else {
    return (x)
  }
}


#' converts a string to a numeric if it holds a number, but retains the string if not
#'
#' @param x
#'
#' @return
#' @export
string_gently_to_numeric = function(x) {
  if  (!is.character(x)) return(x)
  
  suppressWarnings(y <- as.numeric(x))
  if (is.na(y)) return(x)
  return(y)
}