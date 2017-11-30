#' Quantile method for Cumulative Concentration Ratios
#' @export

quantile.CCR <- function(CCR.object, q = 0.5)
{
    obs <- CCR.object$obs
    expdist <- CCR.object$expdist

    cumobs <- cumprop(obs)
    cumexp <- cumprop(expdist)

    vec <- q/quantile(cumexp, quantInv(cumobs, q))
    return(vec)
}
