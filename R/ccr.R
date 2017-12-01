#' Cumulative concentration ratio
#' @export
#'

ccr <- function(x, n = NULL, equality = c("perfect", "maximal", "poisson"),
                reps = 999, mean_distribution = TRUE)
{
    if(is.table(x) | is.data.frame(x))
        x <- tabletovector(x)

    if(is.null(n)) n <- length(x)


    if(isTRUE(mean_distribution))
        central_measure <- mean
    else
        central_measure <- median

    c <- sum(x)

    cn <- c < n

    if(equality[1] == "perfect")
    {
        if(isTRUE(cn)) warning("Perfect equality not reasonable: c < n\n")
        expdist <- list(rep(1, n))
    }
    if(equality[1] == "maximal" )
    {
        expdist <- list(maxequality(x))
    }
    if(equality[1] == "poisson")
    {
        poisdists <- mcpois(x, reps = reps)
        print(summary(poisdists))
        expdist <- poisdists$dists
        print(summary(expdist))
    }

    #ccrvec <- ccr_primitive(obs = x, exp = expdist)

    ccrlist <- lapply(expdist, function(exp) ccr_primitive(obs = x, exp = exp))
    print(summary(ccrlist))
    ccrlistmeans <- sapply(ccrlist, mean)
    print(summary(ccrlistmeans))

    ccr <- central_measure(ccrlistmeans)
    print(summary(ccr))

    results <- list(ccr = ccr, expdist = expdist,
                    equality = equality[1], obs = x)

    if(equality[1] == "poisson")
    {
        results$mean <- mean_distribution
        results$poisdists <- poisdists
    }

    class(results) <- "CCR"

    return(results)
}
