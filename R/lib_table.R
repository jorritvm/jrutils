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


#' open a data.frame in excel
#'
#' wtf (short for writetempfile) will write a data.frame to a temp csv file and open it in excel
#'
#' @param x a data.frame to open in excel
#'
#' @return nothing
#' @export
wtf = function (x) {
  SEP = get_native_list_separator()
  
  tempFilePath = paste(tempfile(), ".csv")
  tempPath = dirname(tempFilePath)
  preferredFile = paste(deparse(substitute(x)), ".csv", sep = "")
  preferredFilePath = file.path(tempPath, preferredFile)
  
  if (length(dim(x)) > 2) {
    stop('Too many dimensions')
  }
  if (is.null(dim(x))) {
    x = as.data.frame(x)
  }
  if (is.null(rownames(x))) {
    tmp = 1:nrow(x)
  } else {
    tmp = rownames(x)
  }
  rownames(x) = NULL
  x = data.frame(RowLabels = tmp, x)
  WriteAttempt = try(write.table(
    x,
    file = preferredFilePath,
    quote = TRUE,
    sep = SEP,
    na = "",
    row.names = FALSE,
    qmethod = "double"
  ),
  silent = TRUE)
  if ("try-error" %in% class(WriteAttempt)) {
    write.table(
      x,
      file = tempFilePath,
      append = FALSE,
      quote = TRUE,
      sep = SEP,
      na = "",
      row.names = FALSE,
      qmethod = "double"
    )
    shell.exec(tempFilePath)
  } else {
    shell.exec(preferredFilePath)
  }
}