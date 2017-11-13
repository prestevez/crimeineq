#' Gini variance 2
#' @export

GiniVar2 <- function(x)
{
    n <- length(x)
    k <- 1:n

    # Jackknife variance
    gnk <- sapply(k, function(i) BernSteen(x[-i]))
    Gvar <- sum((gnk - mean(gnk))^2) * ((n-1)/n)
    return(Gvar)
}
