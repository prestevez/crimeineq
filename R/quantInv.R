#' Inverse Quantile function
#' @export

quantInv <- function(distr, value) ecdf(distr)(value)
