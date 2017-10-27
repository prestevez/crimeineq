#' Print Method for Gmc class
#'
#' A method to print the central estimate of a \link{Gmc} class object.
#' @return A vector of length one with with the central Gini
#'      estimate obtained from the Monte Carlo simulation of counts.
#' @param x an object with class \link{Gmc}.
#'

print.Gmc <- function(x)
{
    est <- x$Estimate
    names(est) <- x$estlabel
    print(est)
}
