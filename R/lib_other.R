#' run this to update the sma_r_utils package from a source bundle
#'
#' @export
#' @import devtools
update_jrutils_package = function() {
  devtools_is_installed = "devtools" %in% installed.packages()[,"Package"]
  if (devtools_is_installed) {
    devtools::install_github("jorritvm/jrutils")
    # install.packages(choose.files(default = "O:/ESM/IPL/SMA/Market modelling/04. Tools/Other/sma_R_utils/versions/.", caption = "Select the source package to install", multi = FALSE, filters = Filters[c("gz")]), repos = NULL, type="source")  
  }  else {
    stop("devtools is required but is not installed yet")
  }
}


#' utility function to cat undefined number of arguments and end with trailing newline 
#'
#' @param ... strings to cat 
#'
#' @export
catn = function(...) {
  cat(paste(strftime(Sys.time() , "(%Y%m%d-%H%M%S)") ,..., "\n"))
}


#' utility function to cat undefined number of arguments and end with trailing newline 
#' and leading timestamp, and starting tic()
#' 
#'
#' @param ... strings to cat
#'
#' @export
#' @import tictoc
tik = function(...) {
  tic(paste(...), quiet = FALSE, func.tic = my.msg.tic)
}
## Using tic custom callbacks 
my.msg.tic <- function(tic, msg) {
  outmsg <- paste("(", tstamp(" ", "-", ":"), ") ", msg, sep = "")
}


#' alias for toc() to be more in line with overloaded function tik()
#'
#' @return
#' @export
#' @import tictoc
tok = function() {
  toc(quiet = FALSE, func.toc = my.msg.toc)
}
## Using toc custom callbacks 
my.msg.toc <- function(tic, toc, msg, info) {
  outmsg <- paste("(", tstamp(" ", "-", ":"), ") ", msg, ": ", round(toc - tic, 2), " seconds elapsed", sep = "")
}


#' Test whether the provided vector of package is installed on the users's system
#' Installed packages are loaded. The user is prompted to install non-installed packages.
#'
#' @param packages Character vector of pkg names
#' @param prompt Boolean whether the user should be prompted to install missing packages
#'
#' @return  Boolean indicating if all packages are installed
#' @export
load_packages_robustly <- function(packages, prompt = TRUE) {
  
  my_lib = .libPaths()[1]
  indices = installed.packages()[, "Package"] %in% packages & installed.packages()[, "LibPath"] == my_lib
  installed_packages = installed.packages()[indices, "Package"]
  new_packages = setdiff(packages, installed_packages)
  
  # Load all installed packages
  for (package in installed_packages) {
    do.call('library', list(package = package, lib.loc = my_lib))
  }
  
  # If there are new (uninstalled) packages, ask to install them
  if (length(new_packages) == 0) {
    all_packages_installed = TRUE
    
  } else {
    if (prompt) {
      print("the following packages are not installed:")
      print(new_packages)
      install_new_packages = readline(prompt = "Would you like to install these packages now? (Y/N): ")
      if (tolower(install_new_packages) == "y") {
        for (package in new_packages) {
          print(package)
          do.call("install.packages", list(pkgs = package))
          Sys.sleep(20) # because install pacakges works asynchronously
          do.call('library', list(package = package, lib.loc = .libPaths()[1]))
        }
        all_packages_installed = TRUE
      } else {
        all_packages_installed = FALSE
      }
    } else {
      all_packages_installed = FALSE
    }
  }
  return(all_packages_installed)
}


#' return the list separator character as set up in your system locale
#'
#' @return a character ("," or ";" or ...) 
#' @export
get_native_list_separator = function() {
  result = NULL
  
  if(.Platform$OS.type == "windows") {

    options(show.error.messages = FALSE)
    try({ result = utils::readRegistry(key = "Control Panel\\International\\", hive = "HCU")$sList })
    options(show.error.messages = TRUE)
    if (is.null(result)) {
      result = ","
    }
    
  } else {
    # non windows system - to be implemented later
    result = ","
  }
  return(result)
}


#' create a progress string like "(5/10)"
#'
#' @param i numeric
#' @param tot numeric
#' @param tspace boolean for trailing space
#'
#' @return
#' @export
pt = function(i, tot, tspace = TRUE) {
  pt = paste0("(",i,"/",tot,")")
  if (tspace) pt = paste0(pt, " ")
  return(pt)
}


#' cats a string representation of a variable's structure, 
#' and puts it on the clipboard if desired, to be pasted in roxygen doc
#'
#' @param var the variable you wish to document
#' @param type is it roxygen 'param' or 'return'?
#' @param cb TRUE FALSE : put doc on clipboard
#'
#' @return a string documenting the variable
#' @export
docvar = function(var, type = "return", cb = TRUE) {
  cl = class(var)[1]
  if (cl == "data.table" | cl == "data.frame") {
    s = paste0("#' @", type, " ")
    indent = paste0("#'",
                    paste(rep(" ",nchar(s)-2), collapse = ""))
    s = paste0(s, "a ", cl, " with structure:\n")
    for (col in names(var)) {
      s = paste0(s, indent, "- ",col, ": ", class(var[[col]])[1], "\n")
    }
  }
  
  if (cb) writeClipboard(s)
  
  cat(s)
}


#' count lines of code in R & python files in your project
#'
#' @param path root path of a project
#' @param skip_comments does not count lines starting in '#'
#' @param skip_blanks does not count blank lines (whitespace)
#'
#' @return integer lines of code
#' @export
loc = function(path,
               skip_comments = FALSE,
               skip_blanks = TRUE) {
  # get the regex
  extensions = c(".py", ".r")
  regex = ""
  for (ext in extensions) {
    regex = paste0(ext, "$|", regex)
  }
  regex = substr(regex, 1, nchar(regex) - 1)
  
  # read the files
  loc = 0
  files = list.files(
    path,
    pattern = regex,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    ignore.case = TRUE
  )
  
  # exclude renv code files
  files = grep(pattern = "/renv/", x = files, invert = TRUE, value = TRUE)
  
  # count loc
  for (file in files) {
    lines = readLines(file, warn = FALSE)
    for (line in lines) {
      line = trimws(line)
      comment = substr(line, 1, 1) == "#"
      if (skip_blanks & line == "")
        next
      if (skip_comments & comment)
        next
      loc = loc + 1
    }
  }
  return(loc)
}
