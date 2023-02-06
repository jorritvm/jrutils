#' merge 2 lists of lists together by common key
#'
#' @param list1 first list
#' @param list2 second list
#'
#' @return a merged list where elements of common key are concatenated into a new sublist
#' @export
merge_lists_by_key = function(list1, list2) {
  common_keys = intersect(names(list1), names(list2))  
  new_list = list()
  for (key in common_keys) {
    new_list[[key]] = c(list1[[key]], list2[[key]])
  }
  return(new_list)
}