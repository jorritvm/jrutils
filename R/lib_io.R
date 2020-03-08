#' replaces backslashes with forward slashes in a path on the clipboard
#'
#' @export
#' @import utils
clip_path = function() {
  x = readClipboard()
  x = gsub(pattern = "\\\\", replacement = "/", x = x)
  writeClipboard(x)
}


#' utility function to open path in windows explorer
#'
#' @param path string to folderpath
#'
#' @export
wopen <- function(path){
  y <- gsub("/", "\\\\", path)
  shell(paste0("explorer ", y), wait = FALSE, intern = FALSE)
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
    sep = ",",
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
      sep = ",",
      na = "",
      row.names = FALSE,
      qmethod = "double"
    )
    shell.exec(tempFilePath)
  } else {
    shell.exec(preferredFilePath)
  }
}
