#' Add line of CCR quantile
#' @export
#'

lines.CCR.quant <- function(CCR.quant.object, ...)
{
    q <- CCR.quant.object$q
    qinv <- CCR.quant.object$qinv
    est <- CCR.quant.object$estimate
    qest <- q/est

    lines(x = c(qinv, qinv), y = c(q, qest), ...)

}
