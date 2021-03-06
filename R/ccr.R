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
        expdist <- poisdists$dists
    }

    #ccrvec <- ccr_primitive(obs = x, exp = expdist)

    ccrlist <- lapply(expdist, function(exp) ccr_primitive(obs = x, exp = exp))
    ccrlistmeans <- sapply(ccrlist, mean)

    ccr <- central_measure(ccrlistmeans)

    results <- list(ccr = ccr, expdist = expdist,
                    equality = equality[1], obs = x, ccrvec = ccrlistmeans,
                    mean = mean_distribution)

    class(results) <- "CCR"

    return(results)
}
