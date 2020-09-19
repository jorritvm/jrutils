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


#' returns total lines of R code for all .R files in wd
#'
#' @param wd string reference to a folder
#'
#' @export
count_lines_of_code = function(wd) {
  rfileslist = list.files(path = wd, pattern = ".R$", full.names = TRUE, recursive = FALSE, include.dirs = FALSE)
  rfiles = lapply(rfileslist, read.delim, header=F, quote="\"")
  rfileslength = sum(sapply(rfiles, function(x) { length(x$V1) }))

  print("Total lines of R code in libraries:")
  print(rfileslength)
}
