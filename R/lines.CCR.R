#' Add line of equality to CCR plot
#' @export
#'

lines.CCR <- function(CCR.object, ...)
{
    if(isTRUE(CCR.object$mean))
        central_measure <- mean
    else
        central_measure <- median

    n <- length(CCR.object$obs)
    c <- sum(CCR.object$obs)

    perfect <- rep(1, n)
    Xi <- cumprop(perfect)
    Xi <- c(0, Xi)

    indicator <- closest(CCR.object$ccrvec, central_measure(CCR.object$ccrvec))

    Yprime <- cumprop(CCR.object$expdist[[indicator]])
    Yprime <- c(0, Yprime)

    lines(x = Xi, y = Yprime, ...)

}
