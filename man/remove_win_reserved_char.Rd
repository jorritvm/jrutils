% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lib_string.R
\name{remove_win_reserved_char}
\alias{remove_win_reserved_char}
\title{removes characters from a string that are removed in the windows file paths}
\usage{
remove_win_reserved_char(s, repl = "_")
}
\arguments{
\item{s}{input string}

\item{repl}{replacement character}
}
\value{
string with special characters removed
}
\description{
removes characters from a string that are removed in the windows file paths
}
