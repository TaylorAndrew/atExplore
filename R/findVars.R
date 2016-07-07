#' findVars
#'
#' findVars searching the variable names of a data.frame for matches to a provided character string, and then returns the matching variable name(s).
#'
#' @param data data.frame to search
#' @param str character string to search for in variable names
#' @param ignore.case TRUE/FALSE: Should character case be ignored when searching
#' @param fuzzy If TRUE, a fuzzy search method will be used
#'
#' @return A vector of matching variable names.
#' @export
#'
#' @examples
#' #fakeData <- data.frame(sex = sample(c('male', 'female'), 10, replace=T), smokng = sample(c('yes', 'no'), 10, replace=T))
#' #findVars(data = fakeData, str = 'smoking')
findVars <- function(data,
                     str,
                     ignore.case = TRUE,
                     fuzzy = TRUE) {
  if (getVars == FALSE) {
    if (fuzzy == FALSE)
      j <- grep(str,
                names(data),
                value = TRUE,
                ignore.case = ignore.case)
    if (fuzzy == TRUE)
      j <- agrep(str,
                 names(data),
                 value = TRUE,
                 ignore.case = ignore.case)
  }
  return(j)
}
