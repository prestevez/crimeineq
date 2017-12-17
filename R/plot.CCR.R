#' Plot method for CCR object
#' @export
#'

plot.CCR <- function(CCR.object, ...)
{
    Yi <- cumprop(CCR.object$obs)
    Yi <- c(0, Yi)

    if(isTRUE(CCR.object$mean))
        central_measure <- mean
    else
        central_measure <- median

    indicator <- closest(CCR.object$ccrvec, central_measure(CCR.object$ccrvec))

    Xi <- cumprop(CCR.object$expdist[[indicator]])
    Xi <- c(0, Xi)


    n <- length(CCR.object$obs)
    c <- sum(CCR.object$obs)

    perfect <- rep(1, n)
    cumperfect <- cumprop(perfect)
    cumperfect <- c(0, cumperfect)


    plot(x = cumperfect, y = Yi, type = "l", xaxs = "i", yaxs = "i",
              xlim = c(0,1), ylim = c(0,1), ...)
    lines(x = cumperfect, y = Xi, lty = 2)
}
