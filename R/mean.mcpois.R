#' Mean distribution of a mcpois object
#' @export

mean.mcpois <- function(mcpois.object)
{
    dists <- mcpois.object$dists
    ginis <- sapply(dists, ineq::Gini)
    meangini <- mean(ginis)
    whichmean <- closest(ginis, meangini)

    meandist <- dists[[whichmean]]
    return(meandist)
}
