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


#' return a string timestamp in ISO8601 using the provided separators
#'
#' @param date_hour_sep separator to be used between date and hour segment of the output
#' @param date_sep separator to be used between date components of the output
#' @param hour_sep separator to be used between hour components of the output
#' @param braces boolean to indicate if time stamp should be put between braces
#'
#' @return string timestamp
#' @export
tstamp = function(date_hour_sep = " ", date_sep = "-", hour_sep = ":", braces = TRUE) {
  date_pattern = paste(c("%Y","%m","%d"), collapse = date_sep)
  hour_pattern = paste(c("%H","%M","%S"), collapse = hour_sep)
  full_pattern = paste(c(date_pattern, hour_pattern), collapse = date_hour_sep)
  s = strftime(Sys.time(), full_pattern)
  
  if (braces) { 
    s = paste0("(", s, ")")
  }
  return(s)  
}

#' version of tstamp without any special characters (only numerical characters) so that it can be used in filenames etc.
#'
#' @return numerical timestamp
#' @export
tstampnum = function() {
  return(tstamp("","","",FALSE))
}
