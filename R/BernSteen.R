#' BernSteen Generalised Gini

BernSteen <- function(x)
{
    n <- length(x)
    c <- sum(x)
    G1 <- ineq::Gini(x)
    G <- max(n/c, 1) * (G1 - 1) + 1
    return(G)
}
