#' Gini Variance
#' @export

GiniVar <- function(x, type = c("classic", "maximal"))
{
    n <- length(x)
    k <- 1:n

    if(type[1] == "maximal")
    {
        ginfun <- Gmax
    }
    else
    {
        ginfun <- ineq::Gini
    }

    # Jackknife variance
    gnk <- sapply(k, function(i) ginfun(x[-i]))
    Gvar <- sum((gnk - mean(gnk))^2) * ((n-1)/n)
    return(Gvar)
}
