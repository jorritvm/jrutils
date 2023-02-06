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
#' @param split separation character
#' @param char_string character string
#'
#' @return
#' @export
csv_to_vector = function(char_string, split=",") {
  x = unlist(strsplit(char_string, split))
  if (str_right(char_string,1) == split) x = c(x,"")
  return(x)
}


#' returns a character vector as a comma seperated string
#'
#' @param char_vector character vector
#' @param split separation character
#'
#' @return
#' @export
vector_to_csv = function(char_vector, split=",") {
  return(paste(char_vector, collapse = split))
}


#' looks for the value closest to the provided values, in a vector of provided values, return the position
#'
#' @param look_near_these_values look for values close to these
#' @param in_these_values look in this vector
#'
#' @return the position of the closest values
#' @export
nearest_pos = function(look_near_these_values, in_these_values) {
  out = c()
  for (p in look_near_these_values) {
    x = which.min(abs(p - in_these_values))[1]
    out = c(out, x)
  }
  return(out)
}


#' looks for the value closest to the provided values, in a vector of provided values, return the value
#'
#' @param look_near_these_values look for values close to these
#' @param in_these_values look in this vector
#'
#' @return
#' @export
nearest_val = function(look_near_these_values, in_these_values) {
  pos = nearest_pos(look_near_these_values, in_these_values)
  out = in_these_values[pos]
  return(out)
}


#' returns a vector where every element is replaced by its fraction of the total sum of the vector
#'
#' @param x a numerical vector
#'
#' @return a numerical vector containing fractional values (0 <= ... <= 1)
#' @export
fractionalise = function(x) {
  return(x / sum(x))
}


#' rescale a numerical vector using a new total sum value
#'
#' @param x a numerica lvector
#' @param total the new total sum to scale to
#'
#' @return a rescaled numerical vector with total sum = total argument
#' @export
rescale = function(x, total) {
  y = fractionalise(x)
  return(y * total)
}


#' fill NA in vector A with values from vector B
#'
#' @param a vector containing NA to replace
#' @param b vector containing substitute values, must be same length as a
#'
#' @return improved vector a
#' @export
#' @examples
#' a = c(1,NA,3,NA)
#' b = 1:4
#' fill_A_NA_with_B(a,b) 
fill_A_NA_with_B = function(a, b) {
  if (length(a) != length(b)) stop("A and B must be same length")
  nas = which(is.na(a))
  a[nas] = b[nas]
  return(a)
}