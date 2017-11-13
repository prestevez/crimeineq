#' Maximum equality vector
#' @export

maxequality <- function(x)
{
    c <- sum(x)
    n <- length(x)

    if(c >= n)
    {
        warning("Maximum equality is the same as perfect equality because c >= n")
        vector <- rep(1, n)
    }
    else
    {
        victims <- rep(1, c)
        nonvictims <- rep(0 , n - c)

        vector <- c(victims, nonvictims)
    }

    return(vector)
}
