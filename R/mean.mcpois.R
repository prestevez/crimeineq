#' Mean distribution of a mcpois object
#' @export

mean.mcpois <- function(mcpois.object)
{
    meandist <- rowMeans(sapply(mcpois.object$dists, unlist))
    return(meandist)
}
