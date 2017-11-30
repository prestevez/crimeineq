#' A function to get the closest value from a vector
#' @export

closest <- function(x, y)
{
    xy <- abs(x - y)
    closest <- which(xy == min(xy))
    names(closest) <- names(y)

    return(closest)
}
