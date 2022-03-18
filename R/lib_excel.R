#' convert an excel cell reference to a list of row and number integers
#'
#' @param s excel cell reference as string
#'
#' @return list(R = ..., C = ...)
#' @export
excel_cell_to_RC = function(s) {
  s_upper = toupper(s)
  
  if (str_extract(s_upper, "^[[:alpha:]]{1,5}[[:digit:]]{1,5}$") != s_upper) {
    stop(paste("Provided excel reference is not allowed:", s))  
  }
  
  alpha = str_extract(s_upper, "^[[:alpha:]]{1,5}")
  excel = list()
  excel$R = as.numeric(str_extract(s_upper, "[[:digit:]]{1,5}$"))
  excel$C = letters_to_numbers(alpha)
  return(excel)
}


#' convert excel letter column reference to numeric
#'
#' @param s excel column reference as string (e.g. A, AB, DYX, ...)
#'
#' @return
#' @export
letters_to_numbers = function(s) {
  s_upper <- toupper(s)
  s_split <- unlist(strsplit(s_upper, split=""))
  s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
  numbers <- 26^((length(s_number)-1):0)
  column_number <- sum(s_number * numbers)
  return(column_number)
}


#' open data.frame in temporary excel file
#'
#' create a temp xlsx file with autofilter from a data.frame and open it in excel
#'
#' @param x a data.frame to open in excel
#' @param path folder path where the file will be saved (default = temp folder)
#' @param fn file name (default = sheet name)
#' @param sheet sheet name (default = variable name)
#' @param open_file TRUE/FALSE to open EXCEL after writing file
#'
#' @return nothing
#' @export
wtx = function (x, path = dirname(tempfile()), fn = NULL, sheet = NULL, open_file = TRUE) {
  sh = deparse(substitute(x))
  
  # get the full file path
  if (is.null(fn)) {
    fn = remove_win_reserved_char(sh) 
  } else {
    fn = remove_win_reserved_char(fn) 
  }
  fnprefix = fn
  i = 1
  while (file.exists(file.path(path, paste0(fn, ".xlsx")))) {
    fn = paste0(fnprefix, "_", i)
    i = i + 1
  }
  fpfn = file.path(path, paste0(fn, ".xlsx"))
  
  
  # clean up input file
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
  
  # write file
  wb = createWorkbook()
  if (!is.null(sheet)) shname = sheet else shname = sh
  addWorksheet(wb, shname)
  writeDataTable(wb, shname, x)
  saveWorkbook(wb, file = fpfn, overwrite = TRUE)
  
  # open file if desired
  if (open_file) shell.exec(fpfn)
}

#' converts a numeric date (e.g. when reading an excel file) to a iso8601 date string
#' works on windows only
#'
#' @param num numeric representation of date - for Excel on Windows, the origin date is December 30, 1899 for dates after 1900.
#'
#' @return
#' @export
numeric_date_to_iso8601 = function(num) {
  return(
    format(as.Date(num, origin = "1899-12-30"), '%Y-%m-%d')
  )  
}