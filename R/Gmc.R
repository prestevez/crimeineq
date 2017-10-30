#' Monte Carlo Gini Coefficient
#'
#' A function that estimates a Gini coefficient using a Monte Carlo simulation
#' of a Poisson process. It uses the \code{\link[ineq]{Gini}} function from the
#' \code{ineq} package.
#' @return A list of \code{Gmc} class, number of replictes, type of estimate (mean or
#'      median), the simulated distributions and their Gini coefficients.
#' @param x a vector of counts that is to be simulated, or a mean event rate
#'      representing the distribution to be simulated. x can be a table or a dataframe
#'      with a distribution, instead of the whole vector.
#' @param n a number representing how many observations should be generated in
#'      each replicate. Defaults to \code{NULL}, in which case the
#'      \code{length(x)} is used.
#' @param reps number of replicates to generate, defaults to 99.
#' @param conf.level the alpha value of the confidence interval; defaults to
#'      0.95. Only accepts values between 0 and 1, exclusive.
#' @param mean logical idicating whether the estimate reported ought to be
#'      the mean or median of the simulated estimates. Defaults to TRUE (i.e.
#'      the mean)
#' @export
#' @keywords Lorenz Curve, Gini, inequality
#' @examples
#'


Gmc <- function(x, n = NULL, reps = 99, mean = TRUE)
{
    if(is.table(x) | is.data.frame(x))
        x <- tabletovector(x)

    if(is.null(n)) n <- length(x)

    mu <- mean(x)

    # Simulating the distribution
    simdist <- lapply(1:reps, function(x) rpois(n, mu))

    # Calculating their gini coefficients
    ginis <- unlist(lapply(simdist, function(x) ineq::Gini(x)))

    if(mean != TRUE)
    {
        estimate <- "median"
        est <- quantile(ginis, 0.5)
    }
    else
    {
        estimate <- "mean"
        est <- mean(ginis)
    }

    results <- list(Estimate = est, reps = reps,
                    estlabel = estimate[1],
                    simdist = simdist, ginis = ginis)

    class(results) <- "Gmc"
    return(results)
}
