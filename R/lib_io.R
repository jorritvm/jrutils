#' shortcut to put your matrix, data.frame or data.table on your clipboard.
#'
#' @param x R table structure (data.frame, matrix, ...)
#' @param size extend clipboard size if data.table is too large
#'
#' @return
#' @export
write_table_to_clipboard = function(x, size = 1024) {
  write.table(x, paste0("clipboard-",size), sep="\t", row.names=FALSE, col.names=TRUE)
}


#' fread should be able to auto detect CSV seperator, this function makes sure
#' it does so always
#'
#' @param fpfn filepath to csv file
#' @param header TRUE/FALSE to identify if your file has a header
#' @param skip integer amount of rows to skip
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


#' utility function to combine pieces of a path into a nice character path --> better: use file.path()
#'
#' @param ... string pieces to combine into file path
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


#' parses a 2 column csv file into a list, usefull for options csv's
#'
#' @param csv_fpfn flie path to csv file
#' @param header TRUE/FALSE/"auto" to indicate whether CSV file contains header row
#'
#' @return
#' @export
two_column_csv_to_list = function(csv_fpfn, header = "auto") {
  dt = robust_fread(fpfn = csv_fpfn, header)
  li = as.list(dt[[2]])
  names(li) = gsub(pattern = " ", replacement = "_", x = dt[[1]])
  
  return (li)
}


#' parses a 2 column xlsx file into a list, usefull for options files
#'
#' @param xlsx_fpfn xlsx file path
#' @param xlsx_sheet string name of sheet or integer number of sheet to read 
#'
#' @return a list object with settings
#' @export
#' @import openxlsx
two_column_xlsx_to_list = function(xlsx_fpfn, xlsx_sheet) {
  dt = read.xlsx(xlsxFile = xlsx_fpfn, sheet = xlsx_sheet)
  
  li = as.list(dt[[2]])
  names(li) = gsub(pattern = " ", replacement = "_", x = dt[[1]])
  
  return (li)
}


#' will create a backup of a file where the file is suffixed with a timestamp and '.backup'
#'
#' @param fpfn_from file path to file to be backed up
#'
#' @return
#' @export
file_backup = function(fpfn_from) {
  if (!file.exists(fpfn)) {
    warning(paste("File does not exist, no backup created for", fpfn))
  } else {
    fpfn_to = paste0(fpfn, ".", time_stamp("_","-","-"), ".backup")
    file.copy(from = fpfn_from, to = fpfn_to, copy.mode = TRUE, copy.date = TRUE) 
  }
}