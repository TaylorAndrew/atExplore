#simple function to count the number of non-NA responses
#' n_
#'
#' n_ returns the length of a vector, excluding NA, NaN, and Infinite values.
#'
#' @param x A vector
#'
#' @return Length of `x`, ignoring NA, NaN, and Infinite values
#' @export
#'
#' @examples
#' #example_vector <- sample(c(1,2,3,NA, Inf, NaN), 100, replace = T)
#' #length(example_vector)
#' #n_(example_vector)
n_ <- function(x) {
  length(x[!is.na(x)&!is.nan(x)&!is.infinite(x)])
}
