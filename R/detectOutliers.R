#' detectOutliers
#'
#' detectOutliers return values of x that are outliers.
#'
#' @param x vector of values
#' @param method how to detect outliers: 'SD', 'Chi', 'Dixon', 'Grubbs', 'Biggest'
#' @param SDs if method='SD', then how many standard deviations from the mean will be considered an outlier boundary
#'
#' @return A vector of outliers from x
#' @export
#'
#' @examples
#' #fakeData <- data.frame(x = rnorm(4, 4, 20))
#' #detectOutliers(x = fakeData$x)
detectOutliers <- function(x, method = "SD", SDs = 3.5) {
  x <- as.numeric(as.character(x))
  methods <- c("SD", "Chi", "Dixon", "Grubbs", "Biggest")
  if (method %in% methods) {
    if (method == "SD") {
      m <- mean(x, na.rm = T)
      s <- sd(x, na.rm = T)
      xOut <- x[(x < (m - (s * SDs)) | x > (m + (s * SDs))) &
                  !is.na(x)]
      return(xOut)
    }
    if (method %in% methods[-1]) {
        if (method == "Chi") {
          out <- chisq.out.test(x)$alternative
          p <- chisq.out.test(x)$p.value
          if (p <= 0.05)
            return(out)
        } else if (method == "Dixon") {
          n <- length(x[!is.na(x)])
          if (n < 3 | n > 30) {
            return(print("To use 'Dixon', sample size must be between 3 and 30"))
          } else {
            out <- dixon.test(x)$alternative
            p <- dixon.test(x)$p.value
            if (p <= 0.05)
              return(out)
          }
        } else if (method == "Grubbs") {
          out <- grubbs.test(x)$alternative
          p <- grubbs.test(x)$p.value
          if (p <= 0.05)
            return(out)
        } else if (method == "Biggest") {
          return(outlier(x))
        }
      }
  } else {
    return(print(paste0("Method must match one of: ", paste0(methods,
                                                             collapse = ', '))))
  }
}
