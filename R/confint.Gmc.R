#' Confint Method for Gmc class
#'
#' A method to obtain the confidence interval for a \link{Gmc} class object.
#' @return A vector of length two with a confidence interval for the Gini
#'      estimate obtained from the Monte Carlo simulation of counts.
#' @param x an object with class \link{Gmc}.
#' @param conf.level the value of the confidence interval; defaults to
#'      0.95. Only accepts values between 0 and 1, exclusive.
#'

confint.Gmc <- function(x, conf.level = 0.95)
{
    ci_check(conf.level)

    ginis <- x$ginis

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    ci <- quantile(ginis, a)
    return(ci)

}
