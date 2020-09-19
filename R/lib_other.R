#' run this to update the sma_r_utils package from a source bundle
#'
#' @export
#' @import devtools
update_jrutils_package = function() {
  devtools_is_installed = "devtools" %in% installed.packages()[,"Package"]
  stop("devtools is required but is not installed yet")
  if (devtools_is_installed) {
    devtools::install_github("jorritvm/jrutils")
    # install.packages(choose.files(default = "O:/ESM/IPL/SMA/Market modelling/04. Tools/Other/sma_R_utils/versions/.", caption = "Select the source package to install", multi = FALSE, filters = Filters[c("gz")]), repos = NULL, type="source")  
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


#' utility function to cat undefined number of arguments and end with trailing newline and leading timestamp, and starting tic()
#'
#' @param ... strings to cat
#'
#' @export
#' @import tictoc
tik = function(...) {
  cat(paste(strftime(Sys.time() , "(%Y%m%d-%H%M%S)") ,..., "\n"))
  tic(paste(...))
}

#' alias for toc() to be more in line with overloaded function tik()
#'
#' @return
#' @export
#' @import tictoc
tok = function() {
  toc()
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
    # non winows system - to be implemented later
    result = ","
  }
  return(result)
<<<<<<< HEAD
=======
}

db = function() {
  
  get_native_list_separator()
  print("done")
>>>>>>> 2824b19645b2bd6d0169909eef7cd2a51157ccc9
}