Gmax <- function(x)
{
    c <- sum(x)
    n <- length(x)
    g <- 1 - c/n

    if(g <= 0)
    {
        G <- 0
    }
    else
    {
        G <- g
    }
    return(G)
}

