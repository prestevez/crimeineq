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

    n <- length(CCR.object$obs)
    c <- sum(CCR.object$obs)

    perfect <- rep(1, n)
    Xi <- cumprop(perfect)
    Xi <- c(0, Xi)


    plot(x = Xi, y = Yi, type = "l", xaxs = "i", yaxs = "i",
              xlim = c(0,1), ylim = c(0,1), ...)
}
