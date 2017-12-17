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
    qinv <- quantInv(cumobs, q)

    cumexp <- lapply(expdist, function(x) c(0, cumprop(x)))

    #vec <- q/quantile(cumexp, quantInv(cumobs, q))
    vec <- sapply(cumexp, function(x) q/quantile(x, qinv))

    est <- central_measure(vec)
    names(est) <- names(vec)[1]

    result <- list(estimate = est, vec = vec, q = q, qinv = qinv)
    class(result) <- "CCR.quant"

    return(result)
}
