#' insert a vector of elements in a vector before or after a given element
#'
#' @param v existing vector
#' @param existing_element positional element, should appear exactly once in the vector
#' @param new_elements vector of new elements
#' @param where "before" or "after"
#'
#' @return a vector with the new values merged in at the correct location
#' @export
#' @examples 
#' insert_at_element(c("a","b","e"), "a", c("c","d"), "before")
#' insert_at_element(c("a","b","e"), "a", c("c","d"), "after")
#' insert_at_element(c("a","b","e"), "b", c("c","d"), "before")
#' insert_at_element(c("a","b","e"), "b", c("c","d"), "after")
#' insert_at_element(c("a","b","e"), "e", c("c","d"), "before")
insert_at_element = function(v, existing_element, new_elements, where = "before") {
  pos = which(v == existing_element)
  if (length(pos) == 0) stop("Element not found in vector")
  if (length(pos) > 1)  stop(paste("Element", existing_element, "found multiple times in vector"))
  if (class(existing_element) != class(new_elements)) stop("Existing elements and new elements not of same type")
  
  if (where == "before") {
    if (pos == 1) {
      new = c(new_elements, v) 
    } else {
      new = c(v[1:(pos-1)],
              new_elements,
              v[pos:length(v)])
    }
  } else if (where == "after") {
    new = c(v[1:pos],
            new_elements)
    if (pos < length(v)) new = c(new, v[(pos+1):length(v)])
  } else {
    stop("Argument where should be defined as 'before' or 'after'")  
  }
  
  return(new)
}


#' splits a comma separated character string into a character vector
#'
#' @param char_vector 
#'
#' @return
#' @export
#'
#' @examples
csv_to_vector = function(char_vector, split=",") {
  x = unlist(strsplit(char_vector, split))
  if (str_right(char_vector,1) == split) x = c(x,"")
  return(x)
}