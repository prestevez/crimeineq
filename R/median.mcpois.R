#' Median distribution of a mcpois object
#' @export

median.mcpois <- function(mcpois.object)
{
    dists <- mcpois.object$dists
    ginis <- sapply(dists, ineq::Gini)
    whichmedian <- which(ginis == median(ginis))

    mediandist <- dists[[whichmedian]]

    return(mediandist)
}
