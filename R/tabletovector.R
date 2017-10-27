#' Table to vector
#'
#' Convenience function to take a distribution table and turn it into a
#' vector of counts.
#' @param x either a dataframe where column 1 is the events and column 2 is the frequency,
#'     or a vector specifying the events. X can also be a table of class table.
#' @param y if x is a vector, y must supply a vector of the same length as x with the frequencies
#'     associated with such events.
#'

tabletovector <- function(x, y = NULL)
{
    if(is.table(x))
    {
        x <- data.frame(x)
    }
    if(is.data.frame(x))
    {
        y <- x[,2]
        x <- x[,1]
        x <- as.numeric(levels(x))[x]
    }
    if(is.null(y))
        stop("y must be supplied if x is a vector.")

    vector <- rep(x, y)
    return(vector)
}
