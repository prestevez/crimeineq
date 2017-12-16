#' Cumulative relative frequency
#' @export

cumprop <- function(x, decreasing = TRUE)
{
    #if (is.unsorted(x)) x <- sort(x, decreasing = decreasing)
    x <- sort(x, decreasing = decreasing)
    result <- cumsum(x)/sum(x)

    return(result)
}
