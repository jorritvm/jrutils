#' prints the topleft corner of a 2D array (matrix, data.frame, data.table) to the console
#'
#' @param x a 2D array
#' @param rcnt row count to print, default = 5
#' @param ccnt column count to print, default = rcnt
#'
#' @return nothing
#' @export
#' @name topleft
topleft = function(x, rcnt = 5, ccnt = rcnt) {
  rcnt = min(rcnt, nrow(x))
  ccnt = min(ccnt, ncol(x))
  print(x[1:rcnt, 1:ccnt])
}
#' @rdname topleft
#' @export
tl = topleft # alias


#' prints the topright corner of a 2D array (matrix, data.frame, data.table) to the console
#'
#' @inheritParams topleft
#'
#' @return nothing
#' @export
#' @name topright
topright = function(x, rcnt = 5, ccnt = rcnt) {
  rcnt = min(rcnt, nrow(x))
  ccnt = min(ccnt, ncol(x))
  print(x[1:rcnt, (ncol(x)-ccnt + 1):ncol(x)])
}
#' @rdname topright
#' @export
tr = topright # alias


#' prints the bottomleft corner of a 2D array (matrix, data.frame, data.table) to the console
#'
#' @inheritParams topleft
#'
#' @return nothing
#' @export
#' @name bottomleft
bottomleft = function(x, rcnt = 5, ccnt = rcnt) {
  rcnt = min(rcnt, nrow(x))
  ccnt = min(ccnt, ncol(x))
  print(x[(nrow(x) - rcnt + 1):nrow(x), 1:ccnt])
}
#' @rdname bottomleft
#' @export
bl = bottomleft # alias


#' prints the bottomright corner of a 2D array (matrix, data.frame, data.table) to the console
#'
#' @inheritParams topleft
#'
#' @return nothing
#' @export
#' @name bottomright
bottomright = function(x, rcnt = 5, ccnt = rcnt) {
  rcnt = min(rcnt, nrow(x))
  ccnt = min(ccnt, ncol(x))
  print(x[(nrow(x) - rcnt + 1):nrow(x), (ncol(x)-ccnt + 1):ncol(x)])
}
#' @rdname bottomright
#' @export
br = bottomright # alias


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


#' merge a data.table x and y (left join) and overwrite the 
#' values in x with those in y for the matching columns
#'
#' @param x data.table
#' @param y data.table
#' @param by A vector of shared column names in x and y to merge on. This defaults to the shared key columns between the two tables. If y has no key columns, this defaults to the key of x
#' @param by.x Vectors of column names in x and y to merge on.
#' @param by.y Vectors of column names in x and y to merge on.
#'
#' @return
#' @export
#' @import data.table
merge_overwrite = function(x, y, 
                           by = NULL, by.x = NULL, by.y = NULL) {
  
  # determine the value columns (as opposed to the id columns which are given in the by argument)
  com.cols    = setdiff(
    intersect(
      setdiff(names(x), by.x), 
      setdiff(names(y), by.y)
    ), 
    by)
  com.cols.x  = paste0(com.cols, ".x")
  com.cols.y  = paste0(com.cols, ".y")
  
  # create combined table
  if (!is.null(by)) {
    m = merge(x, y, by = by, all.x = TRUE)  
  } else {
    m = merge(x, y, by.x = by.x, by.y = by.y, all.x = TRUE)  
  }
  
  # overwrite x when new values are present in y
  for (j in seq_along(com.cols)) 
    m[!is.na(get(com.cols.y[j])), (com.cols.x[j]) := get(com.cols.y[j])]
  
  # remove unneeded columns
  m[, (com.cols.y) := NULL]
  
  # rename kept columns
  setnames(m, com.cols.x, com.cols)
  
  return(m)
  
}


#' distributes per row each element of m_distribute over the elements of 
#' m_data pro rata the weight of each element per line in m_data
#'
#' @param m_distribute m x 1 numeric vector 
#' @param m_data x n numeric matrix
#'
#' @return new matrix 
#' @export
#' @examples 
#' m_distribute = c(1,10)
#' m_data = matrix(c(1,2,20,10), nrow = 2, byrow = TRUE)
#' distribute_vector_over_matrix(m_data, m_distribute)
distribute_vector_over_matrix = function(m_data, m_distribute) {
  m_data_sum = rowSums(m_data)                                    
  ratios = m_data / m_data_sum
  m_distributed = apply(ratios, 2, `*`, m_distribute)
  m_final = m_data + m_distributed
  return(m_final)
}


#' creates a new data.table where every list element which is a vector becomes a column
#'
#' @param l list of vectors of length N, or dataframes with nrow = N
#'
#' @return
#' @export
#' @import data.table
list_of_vectors_to_dt = function(l) {
  res = as.data.table(do.call(cbind, l))
  return(res)
}

#' removes tail rows that contain NA values, up until the last row that is fully complete
#'
#' @param dt data.frame
#'
#' @return
#' @export
remove_tail_na = function(dt) {
  out = dt[ seq( max(which(complete.cases(dt))) ) ]
  return(out)
}
