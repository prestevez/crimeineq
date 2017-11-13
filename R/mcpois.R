#' Monte Carlo simluation of a Poisson process
#' @export

mcpois <- function(x, n = NULL, reps = 99, decreasing = TRUE)
{
    if(is.table(x) | is.data.frame(x))
        x <- tabletovector(x)

    if(is.null(n)) n <- length(x)

    mu <- mean(x)

    # Simulating the distribution
    dists <- lapply(1:reps, function(x) rpois(n, mu))
    dists <- lapply(dists, sort, decreasing = decreasing)

    results <- list(dists = dists, reps = reps,
                    n = n, lambda = mu)

    class(results) <- "mcpois"
    return(results)
}
