#' ccr_primitve
#'

ccr_primitive <- function(obs, exp)
{
    if(length(obs) != length(exp))
        stop("Vector of expected distribution is not same length as observed
             \ndistribution. Probably incorrect xprime value.")

    Y <- cumprop(obs)
    X <- cumprop(exp)

    ccrvec <- Y/X
    return(ccrvec)
}
