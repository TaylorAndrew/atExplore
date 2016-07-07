#' findMissings
#'
#' findMissings provides a vector (per variable), containing the observations in the dataset.
#'
#' @param data data containing `vars`
#' @param vars list of variable names to be checked for missing
#' @param idVar id variable which will be printed showing which observations are missing data for vars. If NULL, then row.names() will be printed.
#'
#' @return a list of missing ids/row.names per variable listed in vars
#' @export
#'
#' @examples
#' #missdata <- data.frame(id = 1:1000, C1 = sample(c(1,NA), 1000, replace=TRUE,
#' #                       prob=c(.8,.2)), C2 = sample(c(1,NA), 1000, replace=TRUE, prob=c(.8,.2)))
#' #findMissings(data = missdata, vars = c("C1", "C2"),idVar = "id")
findMissings <- function(data, vars, idVar=NULL){
  if(!is.null(idVar)) list <- lapply(data[vars], function(x) data[, idVar][which(is.na(x))])
  if(is.null(idVar)) list <- lapply(data[vars], function(x) row.names(data)[which(is.na(x))])
  return(list)
}
