#' nnv
#'
#' nnv returns all values in a vector x that cannot be successfully coerced to numeric.
#'
#' @param x An R vector
#'
#' @return A vector of values that could not be coerced to numeric
#' @export
#'
#' @examples
#' #example_vector <- c("2", "9.123", "blue", "<10")
#' #nnv(example_vector)
nnv <- function(x) {
  is.na1 <- is.na(x)
  x2 <- suppressWarnings(as.numeric(as.character(x)))
  is.na2 <- is.na(x2)
  x[is.na2 & !is.na1]
}
