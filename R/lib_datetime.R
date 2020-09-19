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
