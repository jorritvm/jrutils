#' run this to update the sma_r_utils package from a source bundle
#'
#' @export
#' @import devtools
update_jrutils_package = function() {
  devtools_is_installed = "devtools" %in% installed.packages()[,"Package"]
  stop("devtools is required but is not installed yet")
  if (devtools_is_installed) {
    devtools::install_github("jorritvm/jrutils")
    # install.packages(choose.files(default = "O:/ESM/IPL/SMA/Market modelling/04. Tools/Other/sma_R_utils/versions/.", caption = "Select the source package to install", multi = FALSE, filters = Filters[c("gz")]), repos = NULL, type="source")  
  } 
}

#' utility function to cat undefined number of arguments and end with trailing newline 
#'
#' @param ... strings to cat 
#'
#' @export
catn = function(...) {
  cat(paste(strftime(Sys.time() , "(%Y%m%d-%H%M%S)") ,..., "\n"))
}

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

#' provided a vector of string dates in the standard year 2018 it will return a
#' vector of timeids
#'
#' @param datetime string in the format ymd hm
#'
#' @return
#' @export
convert_datetime_to_timeid = function(datetime) {
  result = c()
  for (dt in datetime) {
    dt = ymd_hm(dt)
    tid = as.duration(ymd("2018-01-01") %--% dt) / dhours(1) + 1
    result = c(result, tid)
  }
  return(result)
}