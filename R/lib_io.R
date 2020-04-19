#' shortcut to put your matrix, data.frame or data.table on your clipboard.
#'
#' @param x 
#' @param size extend clipboard size if data.table is too large
#'
#' @return
#' @export
write_table_to_clipboard = function(x, size = 1024) {
  write.table(x, paste0("clipboard-",size), sep="\t", row.names=FALSE, col.names=TRUE)
}

#' returns the number of R code lines in current working directory
#'
#' @return numeric
#' @export
projects_lines_count = function() {
  return(
    sum(
      unlist(
        lapply(list.files(path = getwd(),
                          pattern = ".*\\.R$",
                          full.names = TRUE),
               function(x) length(readLines(x)) 
        )   
      )
    )
  )
}

#' fread should be able to auto detect CSV seperator, this function makes sure
#' it does so always
#'
#' @param fpfn filepath to csv file
#'
#' @return csv file contents as data.table
#' @export
robust_fread = function(fpfn, header = "auto", skip = 0) {
  if (file.exists(fpfn)) {
    x = fread(fpfn, header = header, skip = skip)
    if (ncol(x) == 1) {
      x = fread(fpfn, sep = ",", header = header, skip = skip)
    }
    if (ncol(x) == 1) {
      x = fread(fpfn, sep = ";", header = header, skip = skip)
    }
  } else {
    x = NULL
    warning(paste("File not found:", fpfn))
  }
  return(x)
}  

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


#' utility function to combine pieces of a path into a nice character path
#'
#' @param ... 
#'
#' @return
#' @export
combine_path = function(...) {
  all_pieces = unlist(list(...))
  path = ""
  for (piece in all_pieces) {
    # replace double backslashes with a single forward slash
    piece = gsub(pattern = "\\\\", "/", piece)
    # strip leading and trailing slash from piece
    if (substr(piece, start = 1, stop = 1) == "/") piece = substr(piece, start = 2, stop = nchar(piece))
    if (substr(piece, start = nchar(piece), stop = nchar(piece)) == "/") piece = substr(piece, start = 1, stop = nchar(piece) - 1)
    # combine it into a path
    path = paste0(path, "/", piece)
  }  
  path = substr(path, start = 2, stop = nchar(path))
  return(path)
}
