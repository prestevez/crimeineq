#' Internal Generalised Gini
#'
#' A function that calculates the Generalised Gini coefficient
#' from an observed Gini and a null Gini corresponding to an alternative
#' distribution. For example, when events are sparsely distributed in the
#' population, ie there are more targets than events, the null Gini
#' can correspond to the maximal equality diagonal. Alternatively, the null
#' Gini can represent the distribution expected under chance as predicted
#' by a Monte Carlo simulation.
#' @param G a number depicting a Gini coefficient. Accepts values between
#'          0 and 1 inclusive.
#' @param Gnull a number depicting a null Gini coefficient. Accepts values
#'          between 0 and 1 inclusive.
#' @keywords Lorenz Curve, Gini, inequality
#' @examples
#'
#' # Generalising by adjusting to expected under chance
#' g1 <- ineq::Gini(testdata$extortions)
#'
#' # simulating 99 distributions under chance
#' reps <- lapply(1:99, function(x){rpois(length(testdata$extortions),
#'                                    mean(testdata$extortions))})
#'
#' # Calculating their gini coefficients
#' sim <- unlist(lapply(reps, function(x) ineq::Gini(x)))
#'
#' # Selecting the mean Gini coefficient
#' gnull <- mean(sim)
#'
#' gengini(g1, gnull)
#'
#' # Generalising by adjusting to maximal equality
#'

gengini <- function(G, Gnull)
{
    # Checks
    if(class(G) == "Gmc")
        G <- G$Estimate
    if(class(Gnull) == "Gmc")
        Gnull <- Gnull$Estimate

    sapply(c(G, Gnull), function(x) if(x < 0 | x > 1)
        stop("A paramter is outside the accepted range: 0-1."))

    # Function
    GG <- (G - Gnull)/(1 - Gnull)
    names(GG) <- "Generalised Gini"
    return(GG)
}
