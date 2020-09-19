#' prints the topleft corner of a 2D array (matrix, data.frame, data.table) to the console
#'
#' @param x a 2D array
#' @param rcnt row count to print, default = 5
#' @param ccnt column count to print, default = rcnt
#'
#' @return nothing
#' @export
topleft = function(x, rcnt = 5, ccnt = rcnt) {
  print(x[1:rcnt, 1:ccnt])
}


