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