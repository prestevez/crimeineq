#' Confint method for CCR.quant object
#' @export

confint.CCR.quant <- function(CCR.quant.object, conf.level = 0.95)
{
    ci_check(conf.level)

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    if(length(CCR.quant.object$vec) == 1)
        stop("confint method only enabled for poisson equality CCR at the moment")

    result <- quantile(CCR.quant.object$vec, a)
}
