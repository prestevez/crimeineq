#' Cumulative concentration ratio
#' @export
#'

ccr <- function(x, n = NULL, equality = c("perfect", "maximal", "poisson"),
                reps = 999, mean_distribution = TRUE, xprime = NULL)
{
    if(is.table(x) | is.data.frame(x))
        x <- tabletovector(x)

    if(is.null(n)) n <- length(x)

    c <- sum(x)

    cn <- c < n

    if(equality[1] == "perfect")
    {
        if(isTRUE(cn)) warning("Perfect equality not reasonable: c < n\n")
        expdist <- rep(1, n)
    }
    if(equality[1] == "maximal" )
    {
        expdist <- maxequality(x)
    }
    if(equality[1] == "poisson")
    {
        poisdists <- mcpois(x, reps = reps)

        if(isTRUE(mean_distribution))
        {
            expdist <- mean(poisdists)
            central_measure <- "mean"
        }
        else
        {
            expdist <- median(poisdists)
            central_measure <- "median"
        }
    }

    ccrvec <- ccr_primitive(obs = x, exp = expdist)

    results <- list(ccr = mean(ccrvec), expdist = expdist,
                    equality = equality[1], obs = x)

    if(equality[1] == "poisson")
    {
        results <- append(results, central_measure = central_measure,
                          poisdists = poisdists)
    }

    class(results) <- "CCR"

    return(results)
}
