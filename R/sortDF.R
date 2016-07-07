#' sortDF
#'
#' sortDF is a convenience wrapper for sorting data.frames.
#'
#' @param data data.frame to sort
#' @param decreasing If TRUE, will be sorted in decreasing order
#' @param by Variable name to sort by. If NULL, will sort by the first column
#' @param ... other arguments to be passed to order
#'
#' @return Sorted data.frame
#' @export
#'
#' @examples
#' #example_df <- data.frame(a = sample(letters[1:3], 10, replace = T),
#' #                         b = rnorm(10))
#' #sortDF(example_df)
#' #sortDF(example_df, by = "b")
#' #sortDF(example_df, decreasing = TRUE, by = "b")
#' #sortDF(example_df, by=c("a", "b"))
#' #sortDF(example_df, by=c("b", "a"))
sortDF <- function(data, decreasing = FALSE, by = NULL) {
  f <- function(...)
    order(...,decreasing = decreasing)
  if(is.null(by)) by = 1
  i <- do.call(f,data[by])
  data[i,,drop = FALSE]
}
