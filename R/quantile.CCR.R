#' Quantile method for Cumulative Concentration Ratios
#' @export

quantile.CCR <- function(CCR.object, q = 0.5)
{
    if(isTRUE(CCR.object$mean))
        central_measure <- mean
    else
        central_measure <- median

    obs <- CCR.object$obs
    expdist <- CCR.object$expdist

    cumobs <- c(0, cumprop(obs))
    #cumexp <- c(0, cumprop(expdist))
    cumexp <- lapply(expdist, function(x) c(0, cumprop(x)))

    #vec <- q/quantile(cumexp, quantInv(cumobs, q))
    vec <- sapply(cumobs, function(x) q/quantile(cumexp, quantInv(x, q)))
    result <- list(estimate = central_measure(vec), vec = vec, q = q)

    class(result) <- "CCR.quant"
    return(result)
}
