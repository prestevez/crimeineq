#' Add line of CCR quantile
#' @export
#'

lines.CCR.quant <- function(CCR.quant.object, ...)
{
    q <- CCR.quant.object$q
    est <- CCR.quant.object$estimate
    qest <- q/est

    lines(x = c(qest, qest), y = c(q, qest), ...)

}
