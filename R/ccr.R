#' Cumulative concentration ratio
#' @export
#'

ccr <- function(x, n = NULL, equality = c("perfect", "maximal", "poisson"), reps = 99)
{
    if(is.table(x) | is.data.frame(x))
        x <- tabletovector(x)

    if(is.null(n)) n <- length(x)

    c <- sum(x)

    cn <- c < n

    Y <- cumprop(x)

    if(equality[1] == "perfect")
    {
        if(isTRUE(cn)) warning("Perfect equality not reasonable: c < n")
        X <- (1:n)/n
    }
    if(equality[1] == "maximal" )
    {
        maxdist <- maxequality(x)
        X <- cumprop(maxdist)
    }
    if(equality[1] == "poisson")
    {
        poisdists <- mcpois(x, reps = reps)
        meandist <- mean(poisdists)
        X <- cumprop(poisdists)
    }

    ccrvec <- Y/X

    results <- mean(ccrvec)

    class(results) <- "CCR"

    return(results)
}
