#' show leftmost x characters from string s
#'
#' @param s string
#' @param x integer
#'
#' @return string
#' @export
str_left = function(s, x) {
  return(substr(s, start = 1, stop = x))
}

#' show rightmost x characters from string sTitle
#'
#' @param s string
#' @param x integer
#'
#' @return string
#' @export
str_right = function(s, x) {
  return(substr(s, start = nchar(s) - x + 1, nchar(s)))
}

#' removes all non alphanumerical characters except - and _ from a string
#'
#' @param s input string
#' @param repl replacement character
#'
#' @return string with special characters removed
#' @export
#' @import stringr
remove_all_spec_char = function(s, repl = "_") {
  x = str_replace_all(s, "[^[:alnum:]|\\_|\\-]", repl)
  return(x)
}


#' removes characters from a string that are removed in the windows file paths
#'
#' @param s input string
#' @param repl replacement character
#'
#' @return string with special characters removed
#' @export
#' @import stringr
remove_win_reserved_char = function(s, repl = "_") {
  not_allowed = '[\\/\\\\\\*\\?\\|\\"\\<\\>\\:]'
  x = str_replace_all(s, not_allowed, repl)
  return(x)
}