#' Quantile method for Cumulative Concentration Ratios
#' @export

quantile.CCR <- function(CCR.object, q = 0.5)
{
    obs <- CCR.object$obs
    expdist <- CCR.object$expdist

    cumobs <- c(0, cumprop(obs))
    cumexp <- c(0, cumprop(expdist))

    vec <- q/quantile(cumexp, quantInv(cumobs, q))
    class(vec) <- "CCR.quant"
    return(vec)
}
